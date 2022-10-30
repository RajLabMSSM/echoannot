#' Query Roadmap API
#'
#' Query Roadmap epigenomic annotations (chromatin marks)
#' using a range of genomic coordinates.
#'
#' \href{https://egg2.wustl.edu/roadmap/data/byFileType/chromhmmSegmentations/ChmmModels/coreMarks/jointModel/final/}{
#' ROADMAP file locations.}
#'
#' @param save_dir Directory to store query results in.
#' @param eid Roadmap annotation ID
#' @param as_granges Whether to return query
#' as a \code{data.frame} or \link[GenomicRanges]{GRanges}.
#' @param force_new Overwrite any existing files of the same name.
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
#' @importFrom echodata get_header dt_to_granges
#' @importFrom data.table fread data.table
#' @importFrom echotabix query construct_query 
#' @importFrom rtracklayer import
#' @importFrom GenomicRanges GRangesList mcols
ROADMAP_tabix <- function(eid,
                          query_dat,
                          save_dir = tempdir(),
                          force_new = FALSE,
                          conda_env = "echoR_mini",
                          nThread = 1,
                          verbose = TRUE) {
    
    #### Prepare query_granges ####  
    query_dat <- echotabix::construct_query(query_dat = query_dat, 
                                            style = "UCSC", 
                                            verbose = verbose)
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
    ## Not ideal, but rtracklayer and tabix are unable to query these files 
    ## returns 493652 rows   
    query_path <- file.path(save_dir,
                            paste("ROADMAP_query",eid,"rds",sep="."))  
    if(file.exists(query_path) & 
       isFALSE(force_new)){
        messager("Importing pre-existing file:",query_path,v=verbose)
        gr <- readRDS(query_path)
    } else{
        messager("Downloading Roadmap Chromatin Marks:", eid, v = verbose) 
        #### Query remote tabix file ####  
        ## "works" but only returns 298 rows.
        # dat <- echotabix::query_table(
        #     target_path = URL,
        #     query_granges = query_dat,  
        #     conda_env = conda_env,
        #     nThread = nThread,
        #     verbose = verbose
        # )   
        gr <- echodata::get_header(path = URL,
                                    nrows = -2L,
                                    colnames_only = FALSE,
                                    nThread = nThread) 
        names(gr)[seq_len(4)] <- c("Chrom", "Start", "End", "name")
        gr <- echodata::dt_to_granges(dat = gr,
                                       chrom_col = "Chrom",
                                       start_col = "Start",
                                       end_col = "End",
                                       style = "UCSC",
                                       verbose = verbose)
        GenomicRanges::mcols(gr)$EID <- eid
        GenomicRanges::mcols(gr)$file <- fname   
        #### Save ####
        dir.create(dirname(query_path), 
                   showWarnings = FALSE, recursive = TRUE)
        messager("Saving query ==>",query_path,v=verbose)
        saveRDS(gr, file = query_path)
    }   
    ##### Save as a series of BED files ####
    bed_gz <- echodata::granges_to_bed(grlist = list(gr) |> `names<-`(eid), 
                                       save_dir = save_dir, 
                                       gzip = TRUE,
                                       nThread = nThread,
                                       verbose = verbose)
    #### Add paths as metadata #### 
    gr@metadata <- list(bed_gz=bed_gz)
    tbx_end <- Sys.time()
    messager("BED subset downloaded in", 
             round(tbx_end - tbx_start, 3), "seconds",
        v = verbose) 
    #### Return ####
    return(gr)
}
