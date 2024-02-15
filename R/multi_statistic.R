#' Calculates ANOVA o KW depending on normality test
#'
#' @param df Data frame with parameter to be evaluated
#' @param group_name Name of the experimental groups
#' @param physeq Phyloseq object containing sample data
#' @param alpha Siginifcance level
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom magrittr %>%
multi_statistic <- function(df, group_name, physeq = data, alpha = 0.05) {
  group_vector <- phyloseq::sample_data(physeq)[[group_name]]
  conditional_test <- function(z){
    if(stats::shapiro.test(z)$p.value < alpha){
      # Not normal, aplying kruskal
      kruskal_res <- stats::kruskal.test(z, {{group_vector}})
      cbind(0,kruskal_res$statistic, kruskal_res$p.value)
    }
    else{
      # normal, aplying anova
      anova_res <- stats::aov(z ~ group_vector) %>%
        stats::summary.aov()
      cbind(1, anova_res[[1]][["F value"]][[1]], anova_res[[1]][["Pr(>F)"]][[1]])

    }
  }
  res <- sapply(df, conditional_test) %>%
    `rownames<-`(c("test","statistic", "p.value")) %>%
    t() %>%
    data.frame() %>%
    dplyr::mutate(test = ifelse(test, "AOV", "KW"))
  return(res)
}
