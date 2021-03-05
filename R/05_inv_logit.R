#' Get inverse logit of a value
#'
#' @description The inverse logit of x
#' @param x A number
#' @return the inverse logit of x.
#' @examples
#' inv_logit(0)
#' inv_logit(1)
#' @export
inv_logit <- function(x) {
  rev <- exp(x) / (1 + exp(x))
  return(rev)
}
