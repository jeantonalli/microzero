#' Utility for compositional microbiota
#'
#' @description
#' This function generates a data frame for relative abundance plots
#'
#' @param physeq Phyloseq object with relative abundance in feature table
#' @param taxrank Taxonomic rank to simplify
#' @param groupvar Experimental grouping variable
#' @param perc_threshold Percentage of relative abundance at which taxa will be considered other
#' @param Abundance Name of the column where Abundance is stored
#'
#' @return Melted data frame with taxa filtered by relative abundance threshold
#' @export
#'
#' @examples
#' @importFrom magrittr "%>%"
#' @importFrom data.table ":="
classic_taxa <- function(physeq, taxrank, groupvar, perc_threshold = 1, Abundance = Abundance) {
  df <- phyloseq::tax_glom(physeq = physeq, taxrank = deparse(substitute(taxrank)), NArm = T)  %>%
    phyloseq::psmelt() %>%
    dplyr::group_by({{groupvar}} ,
             {{taxrank}} ) %>%
    dplyr::mutate(mean = mean(Abundance),
           Classification = {{taxrank}},
           {{taxrank}} := ifelse(mean < perc_threshold, "Other", {{taxrank}}))
  return(df)
}
