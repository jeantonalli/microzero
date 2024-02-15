#' Calculates Geometric mean
#'
#' @param x Data value
#' @param na.rm Should conserve NAs?
#'
#' @return
#' @export
#'
#' @examples
gm_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}
