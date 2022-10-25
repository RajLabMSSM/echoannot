#' Query Roadmap API
#'
#' Query Roadmap epigenomic annotations (chromatin marks)
#' using a range of genomic coordinates.
#'
#' \href{https://egg2.wustl.edu/roadmap/data/byFileType/chromhmmSegmentations/ChmmModels/coreMarks/jointModel/final/}{
#' ROADMAP file locations.}
#'
#' @param results_path Where to store query results.
#' @param eid Roadmap annotation ID
#' @param as_granges Whether to return query
#' as a \code{data.frame} or \link[GenomicRanges]{GRanges}.
#' @inheritParams echotabix::construct_query
#' @source
#' \code{
#' query_dat <- echodata::BST1
#' dat <- ROADMAP_tabix(
#'     query_dat = query_dat,
#'     eid = "E099")
#' }
#' @family ROADMAP
#' @keywords internal
#' @importFrom echodata get_header
#' @importFrom data.table fread data.table
#' @importFrom echotabix query construct_query 
#' @importFrom rtracklayer import
ROADMAP_tabix <- function(eid,
                          query_dat,
                          save_dir = tempdir(),
                          conda_env = "echoR_mini",
                          nThread = 1,
                          verbose = TRUE) {
    
    #### Prepare query_granges ####  
    query_dat <- echotabix::construct_query(query_dat = query_dat, 
                                            style = "UCSC")
    #### Set up save path ####
    fname <- paste0(eid, "_15_coreMarks_dense.bed.bgz")
    URL <- paste(
        "https://egg2.wustl.edu/roadmap/data/byFileType",
        "chromhmmSegmentations/ChmmModels/coreMarks/jointModel/final",
        fname,
        sep="/"
    ) # _15_coreMarks_stateno.bed.gz
    #### Start timer ####
    tbx_start <- Sys.time()
    messager("Downloading Roadmap Chromatin Marks:", eid, v = verbose) 
    #### Not ideal, but rtracklayer and tabix are unable to query these files ####  
    ## returns 493652 rows  
    dat <- echodata::get_header(path = URL,
                                nrows = -2L,
                                colnames_only = FALSE,
                                nThread = nThread)
    #### Query remote tabix file ####  
    ## "works" but only returns 298 rows.
    # dat <- echotabix::query_table(
    #     target_path = URL,
    #     query_granges = query_dat,  
    #     conda_env = conda_env,
    #     nThread = nThread,
    #     verbose = verbose
    # )  
    names(dat)[seq_len(4)] <- c("Chrom", "Start", "End", "name")
    dat <- echodata::dt_to_granges(dat = dat,
                                   chrom_col = "Chrom",
                                   start_col = "Start",
                                   end_col = "End",
                                   style = "UCSC",
                                   verbose = verbose)
    GenomicRanges::mcols(dat)$EID <- eid
    GenomicRanges::mcols(dat)$file <- fname 
    bed_path <- file.path(tempfile(),
                              paste("query",basename(URL),sep=".")) 
    grlist <- list(dat)|> `names<-`(gsub("\\.bed|\\.bgz","",basename(URL)))
    bed_gz <- echodata::granges_to_bed(grlist = grlist, 
                                       save_dir = dirname(bed_path), 
                                       gzip = TRUE,
                                       nThread = nThread,
                                       verbose = verbose)
    dat@metadata <- list(bed_gz=bed_gz)
    tbx_end <- Sys.time()
    messager("BED subset downloaded in", 
             round(tbx_end - tbx_start, 3), "seconds",
        v = verbose
    )
    return(dat)
}
