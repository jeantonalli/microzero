#' Performs DESeq analysis for microbiota data using two groups
#'
#' @param physeq Phyloseq object containing count data
#' @param groupvar Grouping variable
#' @param alpha Significance level
#' @param lfc_threshold Log Fold Change cutoff for biological relevance
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom magrittr %>%
#' @importFrom data.table :=
DESEQGroups <- function(physeq, groupvar, alpha = 0.05, lfc_threshold = 5) {
  formula <- stats::as.formula(paste0('~',{{groupvar}}))
  data.ds <- phyloseq::phyloseq_to_deseq2(physeq, formula)
  geoMeans <- apply(DESeq2::counts(data.ds), 1, gm_mean)
  data.ds <- DESeq2::estimateSizeFactors(data.ds, geoMeans = geoMeans)
  data.ds <- DESeq2::DESeq(data.ds, fitType = 'local')
  res <- DESeq2::results(data.ds)
  res <- res[order(res$padj, na.last=NA), ]
  sigtab <- cbind(methods::as(res, 'data.frame'),
                  methods::as(phyloseq::tax_table(physeq)[rownames(res),
                  ], 'matrix'))
  groups <- levels(phyloseq::sample_data(physeq)[[{{groupvar}}]])
  sigtabgen <- subset(sigtab, !is.na(Genus)) %>%
    dplyr::mutate(group = dplyr::case_when(
      padj < alpha & log2FoldChange > lfc_threshold ~ groups[1],
      padj < alpha & log2FoldChange < -lfc_threshold ~ groups[2]
    ))
  return(sigtabgen)
}
