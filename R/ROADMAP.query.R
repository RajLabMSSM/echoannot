#' Query Roadmap by genomic coordinates
#'
#' @param gr.snp \code{\link[GenomicRanges]{GRanges}} object of 
#' SNPs to query Roadmap with.
#' @param limit_files Limit the number of annotation files queried 
#' (for faster testing).
#' @param nThread Number of threads to parallelise queries over.
#' @inheritParams  ROADMAP.tabix
#' 
#' @family ROADMAP
#' @examples
#' \dontrun{
#' grl.roadmap <- ROADMAP.query(
#'     gr.snp = echodata::BST1,
#'     keyword_query = "placenta")
#' }
#' @export
#' @importFrom GenomicRanges seqnames GRanges start end
ROADMAP.query <- function(results_path = file.path(tempdir(), "Roadmap"),
                          gr.snp,
                          keyword_query = NULL,
                          limit_files = NULL, 
                          nThread = 1,
                          verbose = TRUE) {
    rm_start <- Sys.time() 
    gr.snp <- dt_to_granges(subset_DT = gr.snp, 
                            verbose = verbose)
    roadmap_ref <- ROADMAP.construct_reference(keyword_query = keyword_query)
    if (!is.null(limit_files)) {
        roadmap_ref <- roadmap_ref[seq(1,limit_files), ]
    }
    # Download via tabix (fast)
    counter <- 1
    gr.roadmap <- parallel::mclapply(unique(roadmap_ref$EID),
        function(eid,
                 gr.snp. = gr.snp,
                 results_path. = results_path) {
            message_parallel("+ ROADMAP:: Querying subset from Roadmap API: ",
                eid, " - ", counter, "/", length(unique(roadmap_ref$EID)) )
            counter <<- counter + 1
            dat <- GenomicRanges::GRanges()
            try({
                dat <- ROADMAP.tabix(
                    results_path = results_path.,
                    chrom = as.character(GenomicRanges::seqnames(gr.snp.)[1]),
                    min_pos = min(GenomicRanges::start(gr.snp.), na.rm = TRUE),
                    max_pos = max(GenomicRanges::end(gr.snp.), na.rm = TRUE),
                    eid = eid,
                    convert_to_granges = TRUE)
            })
            if (length(GenomicRanges::seqnames(dat)) > 0) {
                return(dat)
            } else {
                return(NULL)
            }
        },
        mc.cores = nThread
    )
    remove(counter)
    grl.roadmap <- GR.name_filter_convert(
        GR.final = gr.roadmap,
        GR.names =
            roadmap_ref$`Epigenome name (from EDACC Release 9 directory)`,
        min_hits = 1
    )
    rm_end <- Sys.time()
    messager("ROADMAP:: All downloads complete")
    print(round(rm_end - rm_start, 1))
    return(grl.roadmap)
}
