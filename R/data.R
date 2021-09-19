#' Metadata and links to data
#'
#' Metadata for cell type-specific epigenomic bigWig files hosted
#'  on UCSC Genome Browser.
#' bigWig files contain the genomic ranges from each epigenomic assay,
#' as well as a Score column which describes the peaks of the aggregate reads.
#' @family NOTT_2019
#' @source \url{https://science.sciencemag.org/content/366/6469/1134}
#' @examples
#' \dontrun{
#' NOTT_2019.bigwig_metadata <- data.table::data.table(
#'     readxl::read_excel(
#'         file.path(
#'             "~/Desktop/Fine_Mapping/echolocatoR/annotations",
#'             "Nott_2019/Nott_2019.snEpigenomics.xlsx"
#'         )
#'     )
#' )
#' usethis::use_data(NOTT_2019.bigwig_metadata, overwrite = TRUE)
#' }
"NOTT_2019.bigwig_metadata"
