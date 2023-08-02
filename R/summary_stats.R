#' Put mean, sd and hypothesis test in a data frame
#'
#' @param df Data frame
#' @param group_var Experimental groups
#' @param physeq Phyloseq object
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom magrittr "%>%"
summary_stats <- function(df, group_var, physeq = data){
  summary_df <- df %>%
    cbind(group = phyloseq::sample_data(physeq)[[group_var]]) %>%
    dplyr::group_by(group) %>%
    dplyr::summarise_all(mean_sd) %>%
    dplyr::mutate(group = paste0(group, "_mean")) %>%
    tibble::column_to_rownames(var = "group") %>%
    t() %>%
    cbind(multi_statistic(df, group_var))
  return(summary_df)
}
