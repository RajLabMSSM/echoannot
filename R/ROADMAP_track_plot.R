#' Plot ROADMAP query
#'
#' @param grl.roadmap.filt ROADMAP query results.
#' @param gr.snp Optionally, can include an extra \link[GenomicRanges]{GRanges}
#'  object to ensure the plot does not extend beyond certain coordinates.
#' @param geom The type of plot to create.
#' Options include "density" and "histogram".
#' @param adjust The granularity of the peaks.
#' @param show_plot Whether to print the plot.
#' @source
#' \code{
#' gr.snp <- echodata::dt_to_granges(echodata::BST1)
#' grl.roadmap <- ROADMAP_query(
#'     gr.snp = gr.snp,
#'     keyword_query = "monocyte"
#' )
#' grl.roadmap.filt <- ROADMAP_merge_and_process_grl(
#'     grl.roadmap = grl.roadmap,
#'     gr.snp = gr.snp
#' )
#' track.roadmap <- ROADMAP_track_plot(grl.roadmap.filt,
#'     gr.snp = gr.snp
#' )
#' }
#' @keywords internal
#' @importFrom ggbio autoplot
#' @importFrom ggplot2 aes theme_classic theme  element_text
#' @importFrom ggplot2 guides guide_legend scale_y_continuous
#' @importFrom methods show
ROADMAP_track_plot <- function(grl.roadmap.filt,
                               gr.snp = NULL,
                               geom = "density",
                               adjust = .2,
                               show_plot = TRUE,
                               as_ggplot = TRUE,
                               verbose = TRUE) {
    
    ChromState <- NULL;
    messager("Generating ROADMAP track plot.", v = verbose)
    track.roadmap <- ggbio::autoplot(grl.roadmap.filt,
        which = gr.snp,
        ggplot2::aes(fill = ChromState),
        color = "white",
        size = .1,
        geom = geom,
        adjust = adjust,
        # bins=10,
        position = "stack", # stack, fill, dodge
        facets = Source ~ .,
        alpha = 1
    ) +
        ggplot2::theme_classic() +
        ggplot2::theme(
            strip.text.y = ggplot2::element_text(angle = 0),
            strip.text = ggplot2::element_text(size = 9)
        ) +
        ggplot2::guides(fill = ggplot2::guide_legend(
            ncol = 2,
            keyheight = .5,
            keywidth = .5
        )) +
        ggplot2::scale_y_continuous(n.breaks = 3)
    if (show_plot) {
        methods::show(track.roadmap)
    }
    if (as_ggplot) {
        return(track.roadmap@ggplot)
    } else {
        return(track.roadmap)
    }
}
