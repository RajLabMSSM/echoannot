#' Query Roadmap API
#'
#' Query Roadmap epigenomic annotations (chromatin marks)
#' using a range of genomic coordinates.
#'
#' \href{https://egg2.wustl.edu/roadmap/data/byFileType/chromhmmSegmentations/ChmmModels/coreMarks/jointModel/final/}{
#' ROADMAP file locations.}
#'
#' @param results_path Where to store query results.
#' @param chrom Chromosome to query
#' @param min_pos Minimum genomic position
#' @param max_pos Maximum genomic position
#' @param eid Roadmap annotation ID
#' @param convert_to_granges Whether to return query
#' as a \code{data.frame} or \code{\link[GenomicRanges]{GRanges}}.
#'
#' @examples
#' \dontrun{
#' BST1 <- echodata::BST1
#' dat <- ROADMAP_tabix(
#'     chrom = BST1$CHR[1],
#'     min_pos = min(BST1$POS),
#'     max_pos = max(BST1$POS),
#'     eid = "E099"
#' )
#' }
#'
#' @family ROADMAP
#' @keywords internal
#' @importFrom data.table fread
#' @importFrom echotabix query
#' @importFrom echodata dt_to_granges
ROADMAP_tabix <- function(results_path =
                              tempfile(fileext = "ROADMAP_query.csv.gz"),
                          chrom,
                          min_pos,
                          max_pos,
                          eid,
                          convert_to_granges = TRUE,
                          verbose = TRUE) {
    dir.create(results_path, showWarnings = FALSE, recursive = TRUE)
    chrom <- paste0("chr", gsub("chr", "", tolower(chrom)))
    tbx_start <- Sys.time()
    messager("Downloading Roadmap Chromatin Marks:", eid, v = verbose)
    fname <- paste0(eid, "_15_coreMarks_dense.bed.bgz")
    URL <- file.path(
        "https://egg2.wustl.edu/roadmap/data/byFileType",
        "chromhmmSegmentations/ChmmModels/coreMarks/jointModel/final",
        fname
    ) # _15_coreMarks_stateno.bed.gz
    #### Qiuery remote tabix file ####
    query_granges <- echotabix::construct_query(query_chrom = chrom, 
                                                query_start_pos = min_pos, 
                                                query_end_pos = max_pos)
    dat <- echotabix::query(
        target_path = URL,
        query_granges = query_granges,
        verbose = verbose
    )
    dat <- dat[, paste0("V", seq(1, 4))]
    colnames(dat) <- c("Chrom", "Start", "End", "State")
    dat$EID <- eid
    dat$File <- fname
    if (convert_to_granges) {
        dat <- echodata::dt_to_granges(
            dat = dat,
            chrom_col = "Chrom",
            start_col = "Start",
            end_col = "End",
            style = "NCBI"
        )
    }
    tbx_end <- Sys.time()
    messager("BED subset downloaded in", round(tbx_end - tbx_start, 3), "seconds",
        v = verbose
    )
    return(dat)
}
