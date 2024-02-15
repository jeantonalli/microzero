#' Plot compositional microbiota from data frame
#'
#' @param df Relative Abundance data frame with features filtered
#' @param taxrank Taxonomic rank to plot
#' @param groupvar Experimental Group
#' @param Abundance Column were the relative abundance is stored
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom magrittr %>%
plot_compositional <- function(df, taxrank, groupvar, Abundance = Abundance) {
  df <- dplyr::arrange(df, Abundance) %>%
    dplyr::mutate({{taxrank}} := factor( {{taxrank}}, unique({{taxrank}}) ))
  plot <- ggplot2::ggplot(df, ggplot2::aes(x = {{groupvar}}, y = Abundance,
                         fill = {{taxrank}}, color = {{taxrank}}
  )) +
    ggplot2::geom_bar(position = 'fill', stat = 'identity') +
    ggplot2::scale_fill_viridis_d(option = "turbo") +
    ggplot2::scale_color_viridis_d(option = "turbo") +
    ggplot2::scale_y_continuous(label = scales::percent) +
    ggplot2::labs(x = '') +
    ggplot2::theme(legend.key.size = ggplot2::unit(10, 'pt'))
  return(plot)
}
