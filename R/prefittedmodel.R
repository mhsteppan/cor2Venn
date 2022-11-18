#' Answers to the Big Five Personality Test questionnaire (50 items)
#'
#' Data from items from the International Personality Item Pool (N=19,719)

#'
#' @docType cor2Vennobj
#'
#' @usage prefittedmodel
#'
#' @format An object of class \code{"cross"}; see \code{\link[qtl]{read.cross}}.
#'
#' @keywords cor2Vennobj
#'
#' @references R Package "cor2Venn"
#' (\href{http://openpsychometrics.org/_rawdata/}{Link})
#'
#' @source This is a prefitted cor2Venn-Object for a corrrelation matrix based on answers on the Big Five Personality Test questionnaire (50 items) openly accessible from the International Personality Item Pool
#' \href{http://openpsychometrics.org/_rawdata/}{}
#'
#'
#' @examples
#'
#' cormat<- cor(big5,use="pairwise.complete.obs")
#'
#'
#' oldfit <- prefittedmodel
#' betterfit <- cor2Venn(cormat,Recode=TRUE,cor2dist=T,startingvalues = prefittedmodel)

"prefittedmodel"
