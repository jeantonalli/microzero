#' Loads qiime files into R
#'
#' @description
#' depends on qiime2R
#'
#' @param dir Directory where qiime generated files will be present
#' @param metadata_ext Extension for metadata file
#' @param features Feature table name
#' @param taxonomy Taxonomy table name
#' @param metadata Metadata file name
#' @param tree Tree file name, using rooted-tree by default
#'
#' @return A phyloseq object
#' @export
#'
#' @examples
load_from_qiime <- function(dir = "qiime", metadata_ext = ".txt",
                            features = "table", taxonomy = "taxonomy",
                            metadata = "metadata", tree = "rooted-tree") {

  qza <- ".qza" #set qza extension
  features_route <- paste0(dir, "/", features, qza)
  taxonomy_route <- paste0(dir, "/", taxonomy, qza)
  metadata_route <- paste0(dir, "/", metadata, metadata_ext)
  tree_route <- paste0(dir, "/", tree, qza)
  data <- qiime2R::qza_to_phyloseq(features = features_route,
                          taxonomy = taxonomy_route,
                          metadata = metadata_route,
                          tree = tree_route
  )
  return(data)
}
