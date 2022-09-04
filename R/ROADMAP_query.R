#' Query Roadmap by genomic coordinates
#' 
#' @param limit_files Limit the number of annotation files queried
#' (for faster testing).
#' @param chrom_states Filter results by chromatin states.
#' @param return_paths Return list of paths instead of a
#'  \link[GenomicRanges]{GRangesList}.
#' @param conda_env Conda environment to search for tabix in. 
#' @param nThread Number of threads to parallelise queries over.
#' @param verbose Print messages.
#' @inheritParams ROADMAP_tabix
#' @inheritParams ROADMAP_construct_reference
#' @inheritParams echotabix::construct_query
#'
#' @family ROADMAP
#' @export
#' @importFrom GenomicRanges seqnames GRanges start end
#' @importFrom parallel mclapply
#' @importFrom echodata dt_to_granges
#' @examples
#' query_dat <- echodata::BST1
#' grl <- ROADMAP_query(
#'     query_dat = query_dat,
#'     limit_files = 1,
#'     keyword_query = "placenta")
ROADMAP_query <- function(query_dat,
                          results_path = file.path(tempdir(), "Roadmap"),
                          keyword_query = NULL,
                          limit_files = NULL,
                          chrom_states = NULL,
                          remove_tmps = TRUE,
                          return_paths = FALSE,
                          conda_env = "echoR_mini",
                          nThread = 1,
                          verbose = TRUE) {
    
    rm_start <- Sys.time()
    roadmap_ref <- ROADMAP_construct_reference(keyword_query = keyword_query)
    if (!is.null(limit_files)) {
        roadmap_ref <- roadmap_ref[seq(1, limit_files), ]
    }
    #### Download via tabix (fast) ####
    eid_list <- unique(roadmap_ref$EID)
    grl.roadmap <- parallel::mclapply(seq_len(length(eid_list)),
                                     function(i) {
            eid <- eid_list[i]                             
            message_parallel(
                "Querying subset from Roadmap API: ",
                eid, " - ", i, "/", length(unique(roadmap_ref$EID))
            ) 
            dat <- tryCatch({
                ROADMAP_tabix( 
                    query_dat = query_dat,
                    eid = eid,
                    conda_env = conda_env,
                    verbose = verbose
                )
            }, error = function(e){message(e);NULL}) 
            if (length(GenomicRanges::seqnames(dat)) > 0) {
                return(dat)
            } else {
                return(NULL)
            } 
        }, mc.cores = nThread ) |> `names<-`(eid_list) #### END MCLAPPLY
    #### Filter results ####
    #### Annotate/filter chromatin states ####
    grl.roadmap <- filter_chromatin_states(grl = grl.roadmap, 
                                           chrom_states = chrom_states,
                                           verbose = verbose) 
    grl.roadmap <- name_filter_convert(grl = grl.roadmap) 
    rm_end <- Sys.time()
    messager("ROADMAP:: All downloads complete", v = verbose)
    messager(round(rm_end - rm_start, 1), v = verbose)
    #### Return ####
    if(isTRUE(return_paths)){
        bed_gz <- lapply(grl.roadmap, function(x)x@metadata$bed_gz)
        return(bed_gz)
    } else {
        return(grl.roadmap)
    } 
}
