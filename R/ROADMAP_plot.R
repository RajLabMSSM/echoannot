#' Plot annotation peaks: ROADMAP
#'
#' Import annotation from \emph{ROADMAP}, filter to only those within the range
#' of \code{dat}, and then plot the peaks.
#' If the annotation has already been downloaded previously, it will be reused.
#'
#' @param n_top Number of top annotations to be plotted
#' (passed to \link[echoannot]{ROADMAP_merge_and_process_grl}). 
#' @param lib_name Name of the data library to use.
#' @param locus_dir Locus-specific directory.
#' @param roadmap_query Search all columns in the Roadmap annotations metadata
#' and only query annotations that contain your keywords.
#' Can provide multiple keywords in list form:
#' \code{c("placenta","liver","monocytes")}
#'
#' @inheritParams XGR_plot
#' @inheritParams ROADMAP_query
#' @inheritParams ROADMAP_track_plot
#' @inheritParams ROADMAP_construct_reference
#' @inheritParams ROADMAP_tabix
#' 
#' @returns A named list containing:
#' \itemize{
#' \item{"data"}{\code{GRanges} object within the queried coordinates.}
#' \item{"plot"}{\code{ggbio} plot.}
#' }
#'
#' @export
#' @importFrom echodata dt_to_granges
#' @examples
#' roadmap_out <- echoannot::ROADMAP_plot(
#'     dat = echodata::BST1[1:1000, ],
#'     roadmap_query = "placenta"
#' )
ROADMAP_plot <- function(dat,
                         roadmap_query,
                         lib_name = "Roadmap.ChromatinMarks_CellTypes",
                         locus_dir = tempdir(),
                         n_top = 5,
                         force_new = FALSE,
                         show_plot = FALSE, 
                         conda_env = "echoR_mini",
                         nThread = 1,
                         verbose = TRUE) {
    messager("echoannot:: Plotting ROADMAP annotations.", v = verbose)
    gr.snp <- echodata::dt_to_granges(dat = dat)
    annot_file <- annotation_file_name(
        locus_dir = locus_dir,
        lib_name = lib_name
    )
    #### Dowload (or import existing) ####
    if (file.exists(annot_file) & force_new == FALSE) {
        messager("+ Saved annotation file detected. Loading...")
        grl.roadmap <- readRDS(annot_file)
    } else {
        grl.roadmap <- ROADMAP_query(
            results_path = dirname(annot_file),
            # Will convert data.table automatically
            query_dat = dat,
            keyword_query = roadmap_query,
            limit_files = NULL,
            conda_env = conda_env,
            nThread = nThread
        )
        saveRDS(grl.roadmap, annot_file)
    }
    #### Filter ####
    grl.roadmap.filt <- ROADMAP_merge_and_process_grl(
        grl.roadmap = grl.roadmap,
        gr.snp = gr.snp,
        n_top_tissues = n_top,
        sep = "\n",
        verbose = verbose
    )
    #### Plot ####
    track.roadmap <- ROADMAP_track_plot(
        grl.roadmap.filt = grl.roadmap.filt,
        gr.snp = gr.snp,
        show_plot = show_plot,
        verbose = verbose
    )
    #### Return ####
    return(list(
        data = grl.roadmap.filt,
        plot = track.roadmap
    ))
}
