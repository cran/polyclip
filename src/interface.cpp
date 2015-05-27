#include "clipper.h"
#include <R.h>
#include <Rdefines.h>

using namespace std;
using namespace ClipperLib;

void CopyToPath(int *x, int *y, int n, ClipperLib::Path &p)
{
  p.clear();
  p.reserve(n);
  for (int i = 0; i < n; i++)
    p.push_back(IntPoint(x[i], y[i]));
}

void CopyFromPath(ClipperLib::Path &p, int *x, int *y, int nmax, int *n)
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

void ScaleToPath(double *x, double *y, int n, ClipperLib::Path &p,
                 double x0, double y0, double eps)
{
  int i;
  cInt cxi, cyi;
  p.clear();
  p.reserve(n);
  for (i = 0; i < n; i++) {
    cxi = (cInt) ((x[i] - x0)/eps);
    cyi = (cInt) ((y[i] - y0)/eps);
    p.push_back(IntPoint(cxi, cyi));
  }
}

void ScaleFromPath(ClipperLib::Path &p, double *x, double *y, int nmax, int *n,
		  double x0, double y0, double eps)
{
  int N;
  *n = N = p.size();
  if(N <= nmax) {
    for (int i = 0; i < N; i++) 
      {
	x[i] = x0 + eps * ((double) p[i].X);
	y[i] = y0 + eps * ((double) p[i].Y);
      }
  }
}

extern "C" {
  SEXP Cclipbool(SEXP A,
		 SEXP B,
		 SEXP pftA,
		 SEXP pftB,
		 SEXP ct,
		 SEXP X0,
                 SEXP Y0,
                 SEXP Eps
		 ){ 
    int nA, nB, i, n, m, mi, mitrue;
    double *x, *y, *xx, *yy;
    SEXP Ai = R_NilValue, Bi = R_NilValue;
    SEXP out, outi, xouti, youti;
    ClipType cliptype;
    PolyFillType filltypeA, filltypeB;
    int ctcode, pftAcode, pftBcode;
    double x0, y0, eps;
    
    // protect arguments from garbage collector    
    PROTECT(A   = AS_LIST(A));
    PROTECT(B   = AS_LIST(B));
    PROTECT(ct  = AS_INTEGER(ct));
    PROTECT(pftA  = AS_INTEGER(pftA));
    PROTECT(pftB  = AS_INTEGER(pftB));
    PROTECT(X0  = AS_NUMERIC(X0));
    PROTECT(Y0  = AS_NUMERIC(Y0));
    PROTECT(Eps = AS_NUMERIC(Eps));

    // lengths of lists
    nA = LENGTH(A);
    nB = LENGTH(B);

    // Initialise object containing n polygons
    Paths polyA(nA), polyB(nB);

    // Get scale parameters
    x0 = *(NUMERIC_POINTER(X0));
    y0 = *(NUMERIC_POINTER(Y0));
    eps = *(NUMERIC_POINTER(Eps));

    // copy data
    for(i = 0; i < nA; i++) {
      Ai = VECTOR_ELT(A, i);
      n = LENGTH(VECTOR_ELT(Ai, 0));
      x = NUMERIC_POINTER(VECTOR_ELT(Ai, 0));
      y = NUMERIC_POINTER(VECTOR_ELT(Ai, 1));
      ScaleToPath(x, y, n, polyA[i], x0, y0, eps);
    }
    for(i = 0; i < nB; i++) {
      Bi = VECTOR_ELT(B, i);
      n = LENGTH(VECTOR_ELT(Bi, 0));
      x = NUMERIC_POINTER(VECTOR_ELT(Bi, 0));
      y = NUMERIC_POINTER(VECTOR_ELT(Bi, 1));
      ScaleToPath(x, y, n, polyB[i], x0, y0, eps);
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
    Paths result;
    c.AddPaths(polyA, ptSubject, true);
    c.AddPaths(polyB, ptClip, true);
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
	PROTECT(xouti = NEW_NUMERIC(mi));
	PROTECT(youti = NEW_NUMERIC(mi));
	xx = NUMERIC_POINTER(xouti);
	yy = NUMERIC_POINTER(youti);
	// copy to output space
	ScaleFromPath(result[i], xx, yy, mi, &mitrue, x0, y0, eps);
	// Put vectors into list
	SET_VECTOR_ELT(outi, 0, xouti);
	SET_VECTOR_ELT(outi, 1, youti);
	SET_VECTOR_ELT(out, i, outi);
      }
    }

    UNPROTECT(9 + 3*m); // 8 arguments + out + m * (outi, xouti, youti)
    return(out);
  }
}

// offset (dilation) operation for closed polygons

extern "C" {
  SEXP Cpolyoffset(SEXP A,
		   SEXP del,
		   SEXP jt,
		   SEXP mlim,
		   SEXP atol,
		   SEXP X0,
		   SEXP Y0,
		   SEXP Eps
		 ){ 
    int nA, i, n, m, mi, mitrue;
    double *x, *y, *xx, *yy;
    SEXP Ai = R_NilValue;
    SEXP out, outi, xouti, youti;
    JoinType jointype;
    int jtcode;
    double delta, miterlimit, arctolerance;
    double x0, y0, eps;
    
    // protect arguments from garbage collector    
    PROTECT(A   = AS_LIST(A));
    PROTECT(del = AS_NUMERIC(del));
    PROTECT(jt  = AS_INTEGER(jt));
    PROTECT(mlim = AS_NUMERIC(mlim));
    PROTECT(atol = AS_NUMERIC(atol));
    PROTECT(X0  = AS_NUMERIC(X0));
    PROTECT(Y0  = AS_NUMERIC(Y0));
    PROTECT(Eps = AS_NUMERIC(Eps));

    // length of list
    nA = LENGTH(A);

    // Initialise object containing nA polygons
    Paths polyA(nA);

    // Get scale parameters
    x0 = *(NUMERIC_POINTER(X0));
    y0 = *(NUMERIC_POINTER(Y0));
    eps = *(NUMERIC_POINTER(Eps));

    // copy data
    for(i = 0; i < nA; i++) {
      Ai = VECTOR_ELT(A, i);
      n = LENGTH(VECTOR_ELT(Ai, 0));
      x = NUMERIC_POINTER(VECTOR_ELT(Ai, 0));
      y = NUMERIC_POINTER(VECTOR_ELT(Ai, 1));
      ScaleToPath(x, y, n, polyA[i], x0, y0, eps);
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

    // get parameters
    delta = *(NUMERIC_POINTER(del));   // absolute distance
    miterlimit = *(NUMERIC_POINTER(mlim));   // multiple of 'delta'
    arctolerance = *(NUMERIC_POINTER(atol));   // absolute distance
    // rescale
    delta = delta/eps;
    arctolerance = arctolerance/eps;

    // perform offset operation
    ClipperOffset co;
    Paths result;
    co.AddPaths(polyA, jointype, etClosedPolygon);
    co.MiterLimit = miterlimit;
    co.ArcTolerance = arctolerance;
    co.Execute(result, delta);

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
	PROTECT(xouti = NEW_NUMERIC(mi));
	PROTECT(youti = NEW_NUMERIC(mi));
	xx = NUMERIC_POINTER(xouti);
	yy = NUMERIC_POINTER(youti);
	// copy to output space
	ScaleFromPath(result[i], xx, yy, mi, &mitrue, x0, y0, eps);
	// Put vectors into list
	SET_VECTOR_ELT(outi, 0, xouti);
	SET_VECTOR_ELT(outi, 1, youti);
	SET_VECTOR_ELT(out, i, outi);
      }
    }

    UNPROTECT(9 + 3*m); // 8 arguments + out + m * (outi, xouti, youti)
    return(out);
  }
}


// offset (dilation) operation for polygonal lines

extern "C" {
  SEXP Clineoffset(SEXP A,
		   SEXP del,
		   SEXP jt,
		   SEXP et,
		   SEXP mlim,
		   SEXP atol,
		   SEXP X0,
		   SEXP Y0,
		   SEXP Eps
	 ){ 
    int nA, i, n, m, mi, mitrue;
    double *x, *y, *xx, *yy;
    SEXP Ai = R_NilValue;
    SEXP out, outi, xouti, youti;
    JoinType jointype;
    EndType endtype;
    int jtcode, etcode;
    double delta, miterlimit, arctolerance;
    double x0, y0, eps;
    
    // protect arguments from garbage collector    
    PROTECT(A   = AS_LIST(A));
    PROTECT(del = AS_NUMERIC(del));
    PROTECT(jt  = AS_INTEGER(jt));
    PROTECT(et  = AS_INTEGER(et));
    PROTECT(mlim = AS_NUMERIC(mlim));
    PROTECT(atol = AS_NUMERIC(atol));
    PROTECT(X0  = AS_NUMERIC(X0));
    PROTECT(Y0  = AS_NUMERIC(Y0));
    PROTECT(Eps = AS_NUMERIC(Eps));

    // length of list
    nA = LENGTH(A);

    // Initialise object containing nA polygonal lines
    Paths polyA(nA);

    // Get scale parameters
    x0 = *(NUMERIC_POINTER(X0));
    y0 = *(NUMERIC_POINTER(Y0));
    eps = *(NUMERIC_POINTER(Eps));

    // copy data
    for(i = 0; i < nA; i++) {
      Ai = VECTOR_ELT(A, i);
      n = LENGTH(VECTOR_ELT(Ai, 0));
      x = NUMERIC_POINTER(VECTOR_ELT(Ai, 0));
      y = NUMERIC_POINTER(VECTOR_ELT(Ai, 1));
      ScaleToPath(x, y, n, polyA[i], x0, y0, eps);
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
      endtype = etClosedPolygon; 
      break;
    case 2:
      endtype = etClosedLine;
      break;
    case 3:
      endtype = etOpenButt;
      break;
    case 4:
      endtype = etOpenSquare;
      break;
    case 5:
      endtype = etOpenRound;
      break;
    default: 
      error("polyclip: unrecognised code for endtype");
    }

    // get parameters
    delta = *(NUMERIC_POINTER(del));   // absolute distance
    miterlimit = *(NUMERIC_POINTER(mlim));   // multiple of 'delta'
    arctolerance = *(NUMERIC_POINTER(atol));   // absolute distance
    // rescale
    delta = delta/eps;
    arctolerance = arctolerance/eps;

    // perform offset operation
    ClipperOffset co;
    Paths result;
    co.AddPaths(polyA, jointype, endtype);
    co.MiterLimit = miterlimit;
    co.ArcTolerance = arctolerance;
    co.Execute(result, delta);

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
	PROTECT(xouti = NEW_NUMERIC(mi));
	PROTECT(youti = NEW_NUMERIC(mi));
	xx = NUMERIC_POINTER(xouti);
	yy = NUMERIC_POINTER(youti);
	// copy to output space
	ScaleFromPath(result[i], xx, yy, mi, &mitrue, x0, y0, eps);
	// Put vectors into list
	SET_VECTOR_ELT(outi, 0, xouti);
	SET_VECTOR_ELT(outi, 1, youti);
	SET_VECTOR_ELT(out, i, outi);
      }
    }

    UNPROTECT(10 + 3*m); // 9 arguments + out + m * (outi, xouti, youti)
    return(out);
  }
}

