#  First.R
#
#  $Revision: 1.1 $ $Date: 2013/10/19 03:06:59 $
#

.onLoad <- function(...) {} 

.onAttach <- function(libname, pkgname) {
  vs <- read.dcf(file=system.file("DESCRIPTION", package="polyclip"),
                 fields="Version")
  msg <- paste("polyclip", vs)
  packageStartupMessage(msg)
  invisible(NULL)
}

  
