\name{polylineoffset}
\alias{polylineoffset}
\title{Polygonal Line Offset}
\description{
  Given a list of polygonal lines, compute the offset region (guard region,
  buffer region, morphological dilation) formed by shifting the
  boundary outwards by a specified distance. 
}
\usage{
 polylineoffset(A, delta,
         \dots,
         eps, x0, y0,
         limit,
         jointype=c("square", "round", "miter"),
         endtype = c("butt", "square", "round"))
}
\arguments{
  \item{A}{Data specifying polygons. See Details.}
  \item{delta}{Distance over which the boundary should be shifted.}
  \item{\dots}{Ignored.}
  \item{eps}{Spatial resolution for coordinates.}
  \item{x0,y0}{Spatial origin for coordinates.}
  \item{limit}{Tolerance or threshold parameter: see Details.}
  \item{jointype}{
    Type of join operation to be performed at each vertex.
    See Details.
  }
  \item{endtype}{
    Type of geometrical operation to be performed at the start and end
    of each line. See Details.
  }
}
\value{
  Data specifying polygons, in the same format as \code{A}.
}
\details{
  This is part of an interface to the polygon-clipping library
  \code{Clipper} written by Angus Johnson.

  Given a list of polygonal lines \code{A},
  the function \code{polylineoffset} computes the offset region
  (also known as the morphological dilation, guard region,
  buffer region, etc) obtained by shifting the boundary of \code{A}
  outward by the distance \code{delta}.

  The argument \code{A} represents a polygonal line (broken line)
  or a list of broken lines. The format is either
  \itemize{
    \item
    a list containing two components \code{x} and \code{y}
    giving the coordinates of successive vertices of the broken line.
    \item
    a \code{list} of  \code{list(x,y)} structures giving
    the coordinates of the vertices of several broken lines.
  }
  Lines may be self-intersecting and different lines may intersect each other.
  Note that calculations are performed in integer arithmetic: see below.
  
  The argument \code{jointype} determines what happens at the vertices
  of each line. See the Examples for illustrations.
  \itemize{
    \item \code{jointype="round"}: a circular arc is generated.
    \item \code{jointype="square"}: the circular arc is
    replaced by a single straight line.
    \item \code{jointype="miter"}: the circular arc is
    omitted entirely, or replaced by a single straight line.
  }
  The argument \code{endtype} determines what happens at the beginning
  and end of each line. See the Examples for illustrations.
  \itemize{
    \item \code{endtype="butt"}: ends are squared off abruptly.
    \item \code{endtype="square"}:
      ends are squared off at distance \code{delta}.
    \item \code{endtype="round"}: ends are replaced by a semicircular arc.
  }

  For vertices of each line, the argument \code{limit} determines the 
  maximum permissible distance between the ideal result
  and the approximate result that is returned.
  \itemize{
    \item \code{jointype="round"}: \code{limit} is the maximum
    permissible distance between the true circular arc and its
    discretised approximation. The default is \code{limit=delta/1000}.
    \item \code{jointype="square"}: \code{limit} is ignored;
    no constraint applies.
    \item \code{jointype="miter"}: \code{limit * delta} is the maximum
    permissible displacement between the original vertex and the
    corresponding offset vertex if the circular arc were to be
    omitted entirely. The default is \code{limit=2}
    which is also the minimum value.
  }

  \bold{Calculations are performed in integer arithmetic}
  after subtracting \code{x0,y0} from the coordinates,
  dividing by \code{eps}, and rounding to the nearest integer.
  Thus, \code{eps} is the effective spatial resolution.
  The default values ensure reasonable accuracy.
}
\author{Angus Johnson.
  Ported to \R by Adrian Baddeley
  \email{Adrian.Baddeley@uwa.edu.au}.
}
\examples{
  A <- list(list(x=c(4,8,8,2,6), y=c(3,3,8,8,6)))

  plot(c(0,10),c(0,10), type="n",
        main="jointype=square, endtype=square", 
        axes=FALSE, xlab="", ylab="")
  lines(A[[1]], col="grey", lwd=3)
  C <- polylineoffset(A, 0.5, jointype="square", endtype="square")
  polygon(C[[1]], lwd=3, border="blue")

  plot(c(0,10),c(0,10), type="n",
        main="jointype=round, endtype=round", 
        axes=FALSE, xlab="", ylab="")
  lines(A[[1]], col="grey", lwd=3)
  C <- polylineoffset(A, 0.5, jointype="round", endtype="round")
  polygon(C[[1]], lwd=3, border="blue")
}
\references{
  Clipper Website: \url{http://www.angusj.com}

  Vatti, B. (1992) A generic solution to polygon clipping.
  \emph{Communications of the ACM} \bold{35} (7) 56--63.
  \url{http://portal.acm.org/citation.cfm?id=129906}

  Agoston, M.K. (2005)
  \emph{Computer graphics and geometric modeling:
    implementation and algorithms.}
  Springer-Verlag.
  \url{http://books.google.com/books?q=vatti+clipping+agoston}

  Chen, X. and McMains, S. (2005)
  Polygon Offsetting by Computing Winding Numbers.
  Paper no. DETC2005-85513 in \emph{Proceedings of IDETC/CIE 2005}
  (ASME 2005 International Design Engineering Technical Conferences
  and Computers and Information in Engineering Conference),
  pp. 565--575
  \url{http://www.me.berkeley.edu/~mcmains/pubs/DAC05OffsetPolygon.pdf}
}
\keyword{spatial}
\keyword{math}
