#' Query Roadmap by genomic coordinates
#'
#' @param gr.snp \code{\link[GenomicRanges]{GRanges}} object of
#' SNPs to query Roadmap with.
#' @param limit_files Limit the number of annotation files queried
#' (for faster testing).
#' @param conda_env Conda environment to search for tabix in.
#' @param remove_tmps Remove temporary files (e.g. "*.tbi").
#' @param nThread Number of threads to parallelise queries over.
#' @param verbose Print messages.
#' @inheritParams ROADMAP_tabix
#' @inheritParams ROADMAP_construct_reference
#'
#' @family ROADMAP
#' @export
#' @importFrom GenomicRanges seqnames GRanges start end
#' @importFrom parallel mclapply
#' @importFrom echodata dt_to_granges
#' @examples
#' grl.roadmap <- echoannot::ROADMAP_query(
#'     gr.snp = echodata::BST1,
#'     keyword_query = "placenta"
#' )
ROADMAP_query <- function(results_path = file.path(tempdir(), "Roadmap"),
                          gr.snp,
                          keyword_query = NULL,
                          limit_files = NULL,
                          conda_env = "echoR",
                          remove_tmps = TRUE,
                          nThread = 1,
                          verbose = TRUE) {
    rm_start <- Sys.time()
    gr.snp <- echodata::dt_to_granges(
        dat = gr.snp,
        verbose = verbose
    )
    roadmap_ref <- ROADMAP_construct_reference(keyword_query = keyword_query)
    if (!is.null(limit_files)) {
        roadmap_ref <- roadmap_ref[seq(1, limit_files), ]
    }
    # Download via tabix (fast)
    counter <- 1
    gr.roadmap <- parallel::mclapply(unique(roadmap_ref$EID),
        function(eid,
                 gr.snp. = gr.snp,
                 results_path. = results_path) {
            message_parallel(
                "+ ROADMAP:: Querying subset from Roadmap API: ",
                eid, " - ", counter, "/", length(unique(roadmap_ref$EID))
            )
            counter <<- counter + 1
            dat <- GenomicRanges::GRanges()
            try({
                dat <- ROADMAP_tabix(
                    results_path = results_path.,
                    chrom = as.character(GenomicRanges::seqnames(gr.snp.)[1]),
                    min_pos = min(GenomicRanges::start(gr.snp.), na.rm = TRUE),
                    max_pos = max(GenomicRanges::end(gr.snp.), na.rm = TRUE),
                    eid = eid,
                    convert_to_granges = TRUE
                )
            })
            if (length(GenomicRanges::seqnames(dat)) > 0) {
                return(dat)
            } else {
                return(NULL)
            }
        },
        mc.cores = nThread
    ) #### END MCLAPPLY
    remove(counter)
    grl.roadmap <- name_filter_convert(
        GR.final = gr.roadmap,
        GR.names =
            roadmap_ref$`Epigenome name (from EDACC Release 9 directory)`,
        min_hits = 1
    )
    #### Remove temp files ####
    if (remove_tmps) {
        tbi <- list.files(
            path = results_path,
            pattern = ".tbi$", 
            full.names = TRUE
        )
        dummy <- suppressWarnings(file.remove(tbi))
    }
    rm_end <- Sys.time()
    messager("ROADMAP:: All downloads complete", v = verbose)
    messager(round(rm_end - rm_start, 1), v = verbose)
    return(grl.roadmap)
}
