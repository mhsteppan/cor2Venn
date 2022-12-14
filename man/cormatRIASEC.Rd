% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cormatRIASEC.R
\docType{data}
\name{cormatRIASEC}
\alias{cormatRIASEC}
\title{Correlation matrix of variables in the Holland Code RIASEC Test}
\format{
An object of class \code{"cross"}; see \code{\link[qtl]{read.cross}}.
}
\source{
\href{http://openpsychometrics.org/_rawdata/}{}
}
\usage{
data(cormatRIASEC)
}
\description{
Data from openpsychometrics.com (N=145,828) - only correlation matrix (lower triangle)
}
\examples{
data(cormatRIASEC)
library(corrplot)
corrplot(cormatRIASEC,method="color")
}
\references{
(\href{http://openpsychometrics.org/_rawdata/}{openpsychoemtrics.org})
}
\keyword{datasets}
