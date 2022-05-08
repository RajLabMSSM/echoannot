#' Metadata and links to data
#'
#' Metadata for cell type-specific epigenomic bigWig files hosted
#'  on UCSC Genome Browser.
#' bigWig files contain the genomic ranges from each epigenomic assay,
#' as well as a Score column which describes the peaks of the aggregate reads.
#' @family NOTT2019
#' @source \url{https://doi.org/10.1126/science.aay0793}
#' @source
#' \code{
#' NOTT2019_bigwig_metadata <- data.table::data.table(
#'     readxl::read_excel(
#'         file.path(
#'             "~/Desktop/Fine_Mapping/echolocatoR/annotations",
#'             "NOTT2019/NOTT2019_snEpigenomics.xlsx"
#'         )
#'     )
#' )
#' usethis::use_data(NOTT2019_bigwig_metadata, overwrite = TRUE)
#' }
#' @usage data("NOTT2019_bigwig_metadata")
"NOTT2019_bigwig_metadata"


#' Example XGR query
#'
#' Example XGR query results from \pkg{XGR} via
#' \link[echoannot]{XGR_download_and_standardize} using the
#' "ENCODE_DNaseI_ClusteredV3_CellTypes" dataset.
#' @family XGR
#' @source
#' \code{
#' xgr_query <- echoannot::XGR_download_and_standardize(
#'     c("ENCODE_DNaseI_ClusteredV3_CellTypes"),
#'     dat = echodata::BST1)
#' }
#' usethis::use_data(xgr_query, overwrite = TRUE)
#' @usage data("xgr_query")
"xgr_query"
