#' Query Roadmap 
#'  
#' Query Roadmap annotations using a set of genomic coordinates.
#' @param chrom_states Filter results by chromatin states.
#' @param return_paths Return list of paths instead of a
#'  \link[GenomicRanges]{GRangesList}.
#' @param merge_and_process Perform filtering and merging of  
#' \link[GenomicRanges]{GRangesList} items.
#' @param n_top The number of top annotation sources (e.g. tissues)
#'  to include, sorted by greatest number of rows
#' (i.e. the number of genomic ranges within the window).
#' @param min_hits Minimum number of hits 
#' (regions overlapping with \code{query_dat})
#'  required to include a given annotation.
#' @param conda_env Conda environment to search for tabix in. 
#' @param nThread Number of threads to parallelise queries over.
#' @param verbose Print messages.
#' @inheritParams ROADMAP_tabix
#' @inheritParams ROADMAP_construct_reference
#' @inheritParams echotabix::construct_query
#'
#' @family ROADMAP
#' @export
#' @importFrom GenomicRanges seqnames GRanges start end split
#' @importFrom parallel mclapply
#' @importFrom echodata dt_to_granges is_granges
#' @examples
#' query_dat <- echodata::BST1
#' grl <- ROADMAP_query(
#'     query_dat = query_dat,
#'     keyword_query = "monocyte")
ROADMAP_query <- function(query_dat,
                          keyword_query = NULL,
                          limit_files = NULL,
                          chrom_states = NULL,
                          n_top = NULL,
                          min_hits = 1,
                          save_dir = file.path(
                              tempdir(), 
                              paste(paste0(
                                  "roadmap_query--",
                                  paste(keyword_query,collapse = "-")),
                                  paste0("n_top--",n_top), 
                                  paste0("limit_files--",limit_files),
                                  paste(chrom_states,collapse = "-"),
                                  sep=".")),  
                          force_new = FALSE, 
                          return_paths = FALSE,
                          merge_and_process = FALSE, 
                          conda_env = "echoR_mini",
                          nThread = 1,
                          verbose = TRUE) {
    
    # echoverseTemplate:::args2vars(ROADMAP_query)
    # echoverseTemplate:::source_all()
    
    rm_start <- Sys.time()
    ref <- ROADMAP_construct_reference(keyword_query = keyword_query,
                                       limit_files = limit_files,
                                       verbose = verbose) 
    #### Download via tabix (fast) ####
    eid_list <- unique(ref$EID)
    grl.roadmap <- parallel::mclapply(
        X = seq_len(length(eid_list)),
        FUN = function(i) {
            eid <- eid_list[[i]]                             
            message_parallel(
                "Querying subset from Roadmap API: ",
                eid, " - ", i, "/", length(eid_list)
            ) 
            gr <- ROADMAP_tabix( 
                query_dat = query_dat,
                eid = eid,
                save_dir = save_dir,
                force_new = force_new,
                conda_env = conda_env, 
                verbose = verbose)
            if (length(GenomicRanges::seqnames(gr)) > 0) {
                return(gr)
            } else {
                return(NULL)
            } 
    }, mc.cores = nThread) #### END MCLAPPLY
    #### Rename to more informative long name ####
    grl.roadmap <- unlist(GenomicRanges::GRangesList(grl.roadmap))
    grl.roadmap <- GenomicRanges::split(x = grl.roadmap, 
                                        f = grl.roadmap$file)
    #### Annotate biological source ####
    grl.roadmap <- ROADMAP_annotate(grlist = grl.roadmap,
                                    ref = ref,  
                                    verbose = verbose) 
    #### Annotate/filter chromatin states ####
    grl.roadmap <- filter_chromatin_states(grl = grl.roadmap, 
                                           chrom_states = chrom_states,
                                           verbose = verbose) 
    #### Filter by number of hits #####
    grl.roadmap <- name_filter_convert(grl = grl.roadmap, 
                                       min_hits = min_hits) 
    #### Filter ####
    if(isTRUE(merge_and_process)){
        grl.roadmap <- ROADMAP_merge_and_process(
            grl.roadmap = grl.roadmap,
            gr.snp = query_dat,
            n_top = n_top, 
            verbose = verbose
        )
    } 
    #### Report ####
    rm_end <- Sys.time()
    messager("ROADMAP:: Done in",
             round(difftime(rm_end,rm_start,units = "min"), 2),"min.",
             v = verbose) 
    #### Return ####
    if(isTRUE(return_paths)){
        if(echodata::is_granges(grl.roadmap)){
            bed_gz <- grl.roadmap@metadata$bed_gz
        } else {
            bed_gz <- lapply(grl.roadmap, function(x)x@metadata$bed_gz)
        } 
        return(bed_gz)
    } else {
        return(grl.roadmap)
    } 
}
