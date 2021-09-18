#' Query and plot Roadmap epigenomic annotations
#'
#' @param subset_DT Data.frame with at least the following columns:
#' \describe{
#' \item{SNP}{SNP RSID}
#' \item{CHR}{chromosome}
#' \item{POS}{position}
#' }
#' @param force_new_query Force a new query from the XGR database.
#' @inheritParams ROADMAP.construct_reference
#' @inheritParams ROADMAP.tabix
#' @inheritParams ROADMAP.merge_and_process_grl
#' 
#' @return A named list containing:
#' \itemize{
#' \item{\code{ggbio} plot}
#' \item{\code{GRanges} object within the queried coordinates}
#' }
#' 
#' @family ROADMAP
#' @examples
#' \dontrun{
#' roadmap_plot_query <- ROADMAP.query_and_plot(
#'     subset_DT = echodata::BST1,
#'     keyword_query = "monocytes")
#' }
#' @export
#' @importFrom GenomeInfoDb seqlevelsStyle
ROADMAP.query_and_plot <- function(subset_DT,
                                   results_path = 
                                       file.path(tempdir(), "Roadmap"),
                                   n_top_tissues = NULL,
                                   keyword_query = NULL,
                                   adjust = .2,
                                   force_new_query = FALSE,
                                   remove_tmps = TRUE,
                                   verbose=TRUE) {
    # Convert subset to GRanges
    if (all(!is_granges(subset_DT))) {
        messager("ROADMAP:: Converting data to GRanges...")
        gr.snp <- GenomicRanges::makeGRangesFromDataFrame(
            dplyr::mutate( subset_DT, 
                           SEQnames = paste0("chr", CHR) 
                           ),
            seqnames.field = "SEQnames",
            start.field = "POS",
            end.field = "POS"
        )
        GenomeInfoDb::seqlevelsStyle(gr.snp) <- "NCBI"
    } else {
        gr.snp <- subset_DT
    }
    # Roadmap query
    lib <- "Roadmap_ChromatinMarks_CellTypes"
    anno_path <- file.path(
        results_path, "annotations",
        paste0("GRanges_", lib, ".rds")
    )
    if (file.exists(anno_path) & force_new_query == FALSE) {
        messager("+ Saved annotation file detected. Loading...")
        grl.roadmap <- readRDS(anno_path)
    } else {
        dir.create(dirname(anno_path),
            showWarnings = FALSE, recursive = TRUE
        )
        grl.roadmap <- ROADMAP.query(
            results_path = results_path,
            gr.snp = gr.snp,
            keyword_query = keyword_query,
            limit_files = NULL,
            verbose = verbose
        )
        save_annotations(
            gr = grl.roadmap,
            anno_path = anno_path,
            libName = lib,
            verbose = verbose
        )
    }
    grl.roadmap.filt <- ROADMAP.merge_and_process_grl(
        grl.roadmap = grl.roadmap,
        gr.snp = gr.snp,
        n_top_tissues = n_top_tissues
    )
    # Plot
    track.roadmap <- ROADMAP.track_plot(
        grl.roadmap.filt = grl.roadmap.filt,
        gr.snp = gr.snp,
        adjust = adjust
    )
    if (remove_tmps) {
        tbi <- list.files(
            path = results_path,
            pattern = ".tbi$", full.names = TRUE
        )
        dummy <- suppressWarnings(file.remove(tbi))
    }
    return(list(
        Roadmap_plot = track.roadmap,
        Roadmap_query = grl.roadmap.filt
    ))
}
