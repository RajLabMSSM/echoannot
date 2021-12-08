#' bulkATACseq peaks from human brain tissue
#'
#' Each row represents an individual peak identified in the bulk ATAC-seq data.
#'
#' Data originally from \href{https://doi.org/10.1038/s41588-020-00721-x}{
#' Corces et al. (bioRxiv)}, as of May 2020.
#' Specifically: \emph{STable2_Features_bulkATAC-seq_Peaks}
#'
#' @family CORCES2020
#' @source \url{https://doi.org/10.1038/s41588-020-00721-x}
#' @examples
#' \dontrun{
#' dat <- readxl::read_excel(
#'     file.path(
#'         "~/Desktop/Fine_Mapping/echolocatoR",
#'         "annotations/Coceres_2020",
#'         "STable2_Features_bulkATAC-seq_Peaks.xlsx"
#'     ),
#'     skip = 18
#' )
#' CORCES2020_bulkATACseq_peaks <- data.table::data.table(dat)
#'
#' #### piggyback ####
#' tmp <- file.path(tempdir(), "CORCES2020_bulkATACseq_peaks.tsv.gz")
#' data.table::fwrite(CORCES2020_bulkATACseq_peaks, tmp, sep = "\t")
#' piggyback::pb_upload(
#'     file = tmp,
#'     repo = "RajLabMSSM/echoannot"
#' )
#' }
#' @export
get_CORCES2020_bulkATACseq_peaks <- function() {
    tmp <- get_data(fname = "CORCES2020_bulkATACseq_peaks.tsv.gz")
    dat <- data.table::fread(tmp, nThread = 1)
    return(dat)
}
