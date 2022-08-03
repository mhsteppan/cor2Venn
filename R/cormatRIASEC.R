#' Correlation matrix of variables in the Holland Code RIASEC Test
#'
#' Data from openpsychometrics.com (N=145,828) - only correlation matrix (lower triangle)

#'
#' @docType data
#'
#' @usage data(cormatRIASEC)
#'
#' @format An object of class \code{"cross"}; see \code{\link[qtl]{read.cross}}.
#'
#' @keywords datasets
#'
#' @references
#' (\href{http://openpsychometrics.org/_rawdata/}{openpsychoemtrics.org})
#'
#' @source \href{http://openpsychometrics.org/_rawdata/}{}
#'
#'
#' @examples
#' data(cormatRIASEC)
#' library(corrplot)
#' corrplot(cormatRIASEC,method="color")

"cormatRIASEC"
