#
# clipper.R
#
# Interface to Clipper C++ code
#
#  $Revision: 1.7 $ $Date: 2013/10/05 10:19:55 $
#

polyclip <- local({

  preprocess <- function(z, eps, x0, y0) {
    list(x = as.integer(round((z$x-x0)/eps)),
         y = as.integer(round((z$y-y0)/eps)))
  }
  postprocess <- function(z, eps, x0, y0) {
    list(x = x0 + eps * z[[1]],
         y = y0 + eps * z[[2]])
  }
  validxy <- function(P) {
    is.list(P) && all(c("x","y") %in% names(P)) &&
    is.vector(P$x) && is.vector(P$y) && length(P$x)==length(P$y)
  }
  validpoly <- function(P) {
    is.list(P) && all(unlist(lapply(P, validxy)))
  }
  xrange <- function(z) { range(z$x) }
  yrange <- function(z) { range(z$y) }
  
  polyclip <-
    function(A, B, 
             op=c("intersection", "union", "minus", "xor"),
             ...,
             eps, x0, y0,
             fillA=c("evenodd", "nonzero", "positive", "negative"),
             fillB=c("evenodd", "nonzero", "positive", "negative")
             ) {
    # validate parameters and convert to integer codes
    op <- match.arg(op)
    fillA <- match.arg(fillA)
    fillB <- match.arg(fillB)
    ct <- match(op, c("intersection", "union", "minus", "xor"))
    pftA <- match(fillA, c("evenodd", "nonzero", "positive", "negative"))
    pftB <- match(fillB, c("evenodd", "nonzero", "positive", "negative"))
    # validate polygons and rescale
    if(!validpoly(A)) {
      if(validxy(A)) A <- list(A) else
      stop("Argument A should be a list of lists, each containing vectors x,y")
    }
    if(!validpoly(B)) {
      if(validxy(B)) B <- list(B) else
      stop("Argument B should be a list of lists, each containing vectors x,y")
    }
    # determine value of 'eps' if missing
    if(missing(eps) || missing(x0) || missing(y0)) {
      xr <- range(range(unlist(lapply(A, xrange))),
                  range(unlist(lapply(B, xrange))))
      yr <- range(range(unlist(lapply(A, yrange))),
                  range(unlist(lapply(B, yrange))))
      if(missing(eps)) eps <- max(diff(xr), diff(yr))/1e9
      if(missing(x0)) x0 <- mean(xr)
      if(missing(y0)) y0 <- mean(yr)
    } 
    # rescale and convert to integers
    A <- lapply(A, preprocess, eps=eps, x0=x0, y0=y0)
    B <- lapply(B, preprocess, eps=eps, x0=x0, y0=y0)
    # call clipper library
    storage.mode(ct) <- storage.mode(pftA) <- storage.mode(pftB) <- "integer"
    ans <- .Call("Cclipbool",
                 A, B, pftA, pftB, ct)
    ans <- lapply(ans, postprocess, eps=eps, x0=x0, y0=y0)
    return(ans)
  }

  polyclip
})

