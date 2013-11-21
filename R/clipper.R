#
# clipper.R
#
# Interface to Clipper C++ code
#
#  $Revision: 1.8 $ $Date: 2013/11/20 04:40:21 $
#

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

polyoffset <-
  function(A, delta, 
           ...,
           eps, x0, y0,
           limit, 
           jointype = c("square", "round", "miter")
           ) {
    # validate parameters and convert to integer codes
    jointype <- match.arg(jointype)
    jt <- match(jointype, c("square", "round", "miter")) 
    # validate polygons and rescale
    if(!validpoly(A)) {
      if(validxy(A)) A <- list(A) else
      stop("Argument A should be a list of lists, each containing vectors x,y")
    }
    # determine value of 'eps' if missing
    if(missing(eps) || missing(x0) || missing(y0)) {
      xr <- range(unlist(lapply(A, xrange)))
      yr <- range(unlist(lapply(A, yrange)))
      if(missing(eps)) eps <- max(diff(xr), diff(yr))/1e9
      if(missing(x0)) x0 <- mean(xr)
      if(missing(y0)) y0 <- mean(yr)
    }
    switch(jointype,
           square = {
             # limit is ignored
             limit <- lim <- 1
           },
           round = {
             # limit is max tolerance (absolute distance)
             if(missing(limit))
               limit <- delta/1000
             lim <- max(0.5, limit/eps)
           },
           miter = {
               # limit is a multiple of delta
             if(missing(limit))
               limit <- 2
             lim <- limit
           })
    # rescale and convert vertex coordinates to integers
    A <- lapply(A, preprocess, eps=eps, x0=x0, y0=y0)
    del <- delta/eps
    # call clipper library
    storage.mode(jt) <- "integer"
    storage.mode(del) <- storage.mode(lim) <- "double"
    ans <- .Call("Cpolyoffset", A, del, jt, lim)
    ans <- lapply(ans, postprocess, eps=eps, x0=x0, y0=y0)
    return(ans)
  }


polylineoffset <-
  function(A, delta, 
           ...,
           eps, x0, y0,
           limit, 
           jointype = c("square", "round", "miter"),
           endtype = c("butt", "square", "round")
           ) {
    # validate parameters and convert to integer codes
    jointype <- match.arg(jointype)
    jt <- match(jointype, c("square", "round", "miter")) 
    et <- match(endtype, c("closed", "butt", "square", "round")) # SIC
    # validate polygons and rescale
    if(!validpoly(A)) {
      if(validxy(A)) A <- list(A) else
      stop("Argument A should be a list of lists, each containing vectors x,y")
    }
    # determine value of 'eps' if missing
    if(missing(eps) || missing(x0) || missing(y0)) {
      xr <- range(unlist(lapply(A, xrange)))
      yr <- range(unlist(lapply(A, yrange)))
      if(missing(eps)) eps <- max(diff(xr), diff(yr))/1e9
      if(missing(x0)) x0 <- mean(xr)
      if(missing(y0)) y0 <- mean(yr)
    }
    switch(jointype,
           square = {
             # limit is ignored
             limit <- lim <- 1
           },
           round = {
             # limit is max tolerance (absolute distance)
             if(missing(limit))
               limit <- delta/1000
             lim <- max(0.5, limit/eps)
           },
           miter = {
               # limit is a multiple of delta
             if(missing(limit))
               limit <- 2
             lim <- limit
           })
    # rescale and convert vertex coordinates to integers
    A <- lapply(A, preprocess, eps=eps, x0=x0, y0=y0)
    del <- delta/eps
    # call clipper library
    storage.mode(jt) <- storage.mode(et) <- "integer"
    storage.mode(del) <- storage.mode(lim) <- "double"
    ans <- .Call("Clineoffset", A, del, jt, et, lim)
    ans <- lapply(ans, postprocess, eps=eps, x0=x0, y0=y0)
    return(ans)
  }

