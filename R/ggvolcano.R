#' Title
#'
#' @param df Data frame containing differential expression results
#' @param xval Value for x axis (Effect Size)
#' @param yval Value for y axis (p adjust)
#' @param group Group Variable
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom magrittr %>%
#' @importFrom data.table :=
ggvolcano <- function(df, xval = log2FoldChange, yval = padj, group = group){
  df %>%
    ggplot2::ggplot(ggplot2::aes(x = {{xval}}, y = -log10({{yval}}), color = {{group}}, alpha = {{group}})) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = -log10(0.05),
               linetype = "dashed", color = "gray50") +
    ggplot2::geom_vline(xintercept = c(-5, 5),
               linetype = "dashed", color = "gray50") +
    ggplot2::scale_color_viridis_d(na.value = "gray50") +
    ggplot2::scale_alpha_manual(values = rep(1,10), na.value = 0.25)
}
