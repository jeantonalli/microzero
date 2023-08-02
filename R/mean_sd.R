#' Calculates mean ± standard deviation
#'
#' @description
#' Used in conjunction with dplyr::summarise_all
#'
#' @param z Column
#'
#' @return mean ± standard deviation
#' @export
#'
#' @examples
mean_sd <- function(z) {
  rmean <- round(mean(z), digits = 2)
  rsd <- round(stats::sd(z), digits = 2)
  both <- paste0(rmean, " \u00b1 ", rsd)
return(both)
}
