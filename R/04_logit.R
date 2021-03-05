#' Logit transform
#' @description Take the logit transform of a value \emph{p}
#'
#' @param p Value to logit-transform
#'
#' @examples
#' x <- logit(p = 0.01)
#' @export
logit <- function(p) {
  r <- log(p / (1 - p))
  return(r)
}
