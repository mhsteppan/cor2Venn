#' Correlation matrix of variables in the IDS2 Intelligence and Development Scales 2
#'
#' Data from  (N=2,030) - only correlation matrix (lower triangle)

#'
#' @docType data
#'
#' @usage data(ids2cormat)
#'
#' @format An object of class \code{"cross"}; see \code{\link[qtl]{read.cross}}.
#'
#' @keywords datasets
#'
#' @references Grieder, S., & Grob, A. (2020). Exploratory factor analyses of the Intelligence and Development Scales–2: Implications for theory and practice. Assessment, 27(8), 1853-1869.
#' (\href{https://www.testzentrale.ch/shop/intelligence-and-development-scales-2-86715.html}{Link})
#'
#' @source \href{https://www.testzentrale.ch/shop/intelligence-and-development-scales-2-86715.html}{}
#'
#'
#' @examples
#' data(ids2cormat)
#' library(corrplot)
#' corrplot(ids2cormat,method="color")

"ids2cormat"
