#include "clipper.h"
#include <R.h>
#include <Rdefines.h>

using namespace std;
using namespace ClipperLib;

void CopyToPoly(int *x, int *y, int n, ClipperLib::Polygon &p)
{
  p.clear();
  p.reserve(n);
  for (int i = 0; i < n; i++)
    p.push_back(IntPoint(x[i], y[i]));
}

void CopyFromPoly(ClipperLib::Polygon &p, int *x, int *y, int nmax, int *n)
{
  int N;
  *n = N = p.size();
  if(N <= nmax) {
    for (int i = 0; i < N; i++) 
      {
	x[i] = p[i].X;
	y[i] = p[i].Y;
      }
  }
}

extern "C" {
  SEXP Cclipbool(SEXP A,
		 SEXP B,
		 SEXP pftA,
		 SEXP pftB,
		 SEXP ct
		 ){ 
    int nA, nB, i, n, m, mi, mitrue;
    int *x, *y, *xx, *yy;
    SEXP Ai = R_NilValue, Bi = R_NilValue;
    SEXP out, outi, xouti, youti;
    ClipType cliptype;
    PolyFillType filltypeA, filltypeB;
    int ctcode, pftAcode, pftBcode;
    
    // protect arguments from garbage collector    
    PROTECT(A   = AS_LIST(A));
    PROTECT(B   = AS_LIST(B));
    PROTECT(ct  = AS_INTEGER(ct));
    PROTECT(pftA  = AS_INTEGER(pftA));
    PROTECT(pftB  = AS_INTEGER(pftB));

    // lengths of lists
    nA = LENGTH(A);
    nB = LENGTH(B);

    // Initialise object containing n polygons
    Polygons polyA(nA), polyB(nB);

    // copy data
    for(i = 0; i < nA; i++) {
      Ai = VECTOR_ELT(A, i);
      n = LENGTH(VECTOR_ELT(Ai, 0));
      x = INTEGER(VECTOR_ELT(Ai, 0));
      y = INTEGER(VECTOR_ELT(Ai, 1));
      CopyToPoly(x, y, n, polyA[i]);
    }
    for(i = 0; i < nB; i++) {
      Bi = VECTOR_ELT(B, i);
      n = LENGTH(VECTOR_ELT(Bi, 0));
      x = INTEGER(VECTOR_ELT(Bi, 0));
      y = INTEGER(VECTOR_ELT(Bi, 1));
      CopyToPoly(x, y, n, polyB[i]);
    }

    // interpret clipping parameters
    ctcode = *(INTEGER_POINTER(ct));
    pftAcode = *(INTEGER_POINTER(pftA));
    pftBcode = *(INTEGER_POINTER(pftB));
    switch(ctcode) {
    case 1: 
      cliptype = ctIntersection; 
      break;
    case 2:
      cliptype = ctUnion;
      break;
    case 3:
      cliptype = ctDifference;
      break;
    case 4:
      cliptype = ctXor;
      break;
    default: 
      error("polyclip: unrecognised code for cliptype");
    }
    switch(pftAcode) {
    case 1: 
      filltypeA = pftEvenOdd; 
      break;
    case 2:
      filltypeA = pftNonZero;
      break;
    case 3:
      filltypeA = pftPositive;
      break;
    case 4:
      filltypeA = pftNegative;
      break;
    default: 
      error("polyclip: unrecognised code for fill type A");
    }
    switch(pftBcode) {
    case 1: 
      filltypeB = pftEvenOdd; 
      break;
    case 2:
      filltypeB = pftNonZero;
      break;
    case 3:
      filltypeB = pftPositive;
      break;
    case 4:
      filltypeB = pftNegative;
      break;
    default: 
      error("polyclip: unrecognised code for fill type B");
    }

    // perform clipping operation
    Clipper c;
    Polygons result;
    c.AddPolygons(polyA, ptSubject);
    c.AddPolygons(polyB, ptClip);
    c.Execute(cliptype, result, filltypeA, filltypeB);

    // number of polygons
    m = result.size();
    
    // initialise output list
    PROTECT(out  = NEW_LIST(m));
    
    // copy data
    if(m > 0) {
      for(i = 0; i < m; i++) {
	mi = result[i].size();
	// Allocate space for output
	PROTECT(outi = NEW_LIST(2));
	PROTECT(xouti = NEW_INTEGER(mi));
	PROTECT(youti = NEW_INTEGER(mi));
	xx = INTEGER_POINTER(xouti);
	yy = INTEGER_POINTER(youti);
	// copy to output space
	CopyFromPoly(result[i], xx, yy, mi, &mitrue);
	// Put vectors into list
	SET_VECTOR_ELT(outi, 0, xouti);
	SET_VECTOR_ELT(outi, 1, youti);
	SET_VECTOR_ELT(out, i, outi);
      }
    }

    UNPROTECT(6 + 3*m); // 5 arguments + out + m * (outi, xouti, youti)
    return(out);
  }
}

// offset (dilation) operation for closed polygons

extern "C" {
  SEXP Cpolyoffset(SEXP A,
		   SEXP del,
		   SEXP jt,
		   SEXP lim
		 ){ 
    int nA, i, n, m, mi, mitrue;
    int *x, *y, *xx, *yy;
    SEXP Ai = R_NilValue;
    SEXP out, outi, xouti, youti;
    JoinType jointype;
    int jtcode;
    double delta, limit;
    
    // protect arguments from garbage collector    
    PROTECT(A   = AS_LIST(A));
    PROTECT(del = AS_NUMERIC(del));
    PROTECT(jt  = AS_INTEGER(jt));
    PROTECT(lim = AS_NUMERIC(lim));

    // length of list
    nA = LENGTH(A);

    // Initialise object containing nA polygons
    Polygons polyA(nA);

    // copy data
    for(i = 0; i < nA; i++) {
      Ai = VECTOR_ELT(A, i);
      n = LENGTH(VECTOR_ELT(Ai, 0));
      x = INTEGER(VECTOR_ELT(Ai, 0));
      y = INTEGER(VECTOR_ELT(Ai, 1));
      CopyToPoly(x, y, n, polyA[i]);
    }

    // interpret offset parameters
    jtcode = *(INTEGER_POINTER(jt));
    switch(jtcode) {
    case 1: 
      jointype = jtSquare; 
      break;
    case 2:
      jointype = jtRound;
      break;
    case 3:
      jointype = jtMiter;
      break;
    default: 
      error("polyclip: unrecognised code for jointype");
    }
    delta = *(NUMERIC_POINTER(del));
    limit = *(NUMERIC_POINTER(lim));

    // perform offset operation
    Polygons result;
    OffsetPolygons(polyA, result, delta, jointype, limit, true);

    // number of polygons
    m = result.size();
    
    // initialise output list
    PROTECT(out  = NEW_LIST(m));
    
    // copy data
    if(m > 0) {
      for(i = 0; i < m; i++) {
	mi = result[i].size();
	// Allocate space for output
	PROTECT(outi = NEW_LIST(2));
	PROTECT(xouti = NEW_INTEGER(mi));
	PROTECT(youti = NEW_INTEGER(mi));
	xx = INTEGER_POINTER(xouti);
	yy = INTEGER_POINTER(youti);
	// copy to output space
	CopyFromPoly(result[i], xx, yy, mi, &mitrue);
	// Put vectors into list
	SET_VECTOR_ELT(outi, 0, xouti);
	SET_VECTOR_ELT(outi, 1, youti);
	SET_VECTOR_ELT(out, i, outi);
      }
    }

    UNPROTECT(5 + 3*m); // 4 arguments + out + m * (outi, xouti, youti)
    return(out);
  }
}


// offset (dilation) operation for polygonal lines

extern "C" {
  SEXP Clineoffset(SEXP A,
		   SEXP del,
		   SEXP jt,
		   SEXP et,
		   SEXP lim
		 ){ 
    int nA, i, n, m, mi, mitrue;
    int *x, *y, *xx, *yy;
    SEXP Ai = R_NilValue;
    SEXP out, outi, xouti, youti;
    JoinType jointype;
    EndType endtype;
    int jtcode, etcode;
    double delta, limit;
    
    // protect arguments from garbage collector    
    PROTECT(A   = AS_LIST(A));
    PROTECT(del = AS_NUMERIC(del));
    PROTECT(jt  = AS_INTEGER(jt));
    PROTECT(et  = AS_INTEGER(et));
    PROTECT(lim = AS_NUMERIC(lim));

    // length of list
    nA = LENGTH(A);

    // Initialise object containing nA polygonal lines
    Polygons polyA(nA);

    // copy data
    for(i = 0; i < nA; i++) {
      Ai = VECTOR_ELT(A, i);
      n = LENGTH(VECTOR_ELT(Ai, 0));
      x = INTEGER(VECTOR_ELT(Ai, 0));
      y = INTEGER(VECTOR_ELT(Ai, 1));
      CopyToPoly(x, y, n, polyA[i]);
    }

    // interpret offset parameters
    jtcode = *(INTEGER_POINTER(jt));
    switch(jtcode) {
    case 1: 
      jointype = jtSquare; 
      break;
    case 2:
      jointype = jtRound;
      break;
    case 3:
      jointype = jtMiter;
      break;
    default: 
      error("polyclip: unrecognised code for jointype");
    }
    etcode = *(INTEGER_POINTER(et));
    switch(etcode) {
    case 1: 
      endtype = etClosed; 
      break;
    case 2:
      endtype = etButt;
      break;
    case 3:
      endtype = etSquare;
      break;
    case 4:
      endtype = etRound;
      break;
    default: 
      error("polyclip: unrecognised code for endtype");
    }
    delta = *(NUMERIC_POINTER(del));
    limit = *(NUMERIC_POINTER(lim));

    // perform offset operation
    Polygons result;
    OffsetPolyLines(polyA, result, delta, jointype, endtype, limit, true);

    // number of polygons
    m = result.size();
    
    // initialise output list
    PROTECT(out  = NEW_LIST(m));
    
    // copy data
    if(m > 0) {
      for(i = 0; i < m; i++) {
	mi = result[i].size();
	// Allocate space for output
	PROTECT(outi = NEW_LIST(2));
	PROTECT(xouti = NEW_INTEGER(mi));
	PROTECT(youti = NEW_INTEGER(mi));
	xx = INTEGER_POINTER(xouti);
	yy = INTEGER_POINTER(youti);
	// copy to output space
	CopyFromPoly(result[i], xx, yy, mi, &mitrue);
	// Put vectors into list
	SET_VECTOR_ELT(outi, 0, xouti);
	SET_VECTOR_ELT(outi, 1, youti);
	SET_VECTOR_ELT(out, i, outi);
      }
    }

    UNPROTECT(6 + 3*m); // 5 arguments + out + m * (outi, xouti, youti)
    return(out);
  }
}


