#' Plot XGR peaks
#'
#' Plots the distribution of annotations across a genomic region (x-axis).
#'
#' @family XGR
#' @keywords internal
#' @param gr.lib \code{GRanges} object of annotations.
#' @param geom Plot type ("density", or "histogram").
#' @param locus Locus name (\emph{optional}).
#' @param adjust The granularity of the peaks.
#' @param show_plot Print the plot.
#' @return \code{ggbio} track plot.
#' @inheritParams XGR.prepare_foreground_background
#' @examples
#' \dontrun{
#' gr.lib <- XGR.download_and_standardize(
#'     c("ENCODE_DNaseI_ClusteredV3_CellTypes"),
#'     finemap_dat = echodata::BST1
#' )
#' gr.filt <- XGR.filter_sources(gr.lib = gr.lib, n_top_sources = 5)
#' gr.filt <- XGR.filter_assays(gr.lib = gr.filt, n_top_assays = 5)
#' xgr.track <- XGR.plot_peaks(
#'     gr.lib = gr.filt,
#'     subset_DT = echodata::BST1,
#'     fill_var = "Assay",
#'     facet_var = "Source"
#' )
#' }
#' @importFrom GenomicRanges mcols
XGR.plot_peaks <- function(gr.lib,
                           subset_DT,
                           fill_var = "Assay",
                           facet_var = "Source",
                           geom = "density",
                           locus = NULL,
                           adjust = .2,
                           show_plot = TRUE,
                           show.legend = TRUE,
                           as.ggplot = TRUE,
                           trim_xlims = FALSE) {
    # data("BST1"); subset_DT <- BST1; show.legend=T;
    # fill_var="Assay"; facet_var="Source"; geom="density"; adjust=.2;
    gr.lib$facet_label <- gsub(
        "_", "\n",
        GenomicRanges::mcols(gr.lib)[, facet_var]
    )
    xgr.track <- ggbio::autoplot(gr.lib,
        # which = gr.snp,
        ggplot2::aes(fill = eval(parse(text = fill_var))),
        # formula(paste0(facet_var," ~ .")),
        facets = formula("facet_label ~ ."),
        # fill = "magenta",
        color = "white", # NA
        geom = geom,
        adjust = adjust,
        position = "stack",
        # bins=50,
        size = .1,
        alpha = .7,
        show.legend = show.legend
    ) +
        ggplot2::theme_bw() +
        ggplot2::labs(fill = fill_var)
    if (trim_xlims) {
        xgr.track <- suppressMessages(
            xgr.track +
                xlim(min(subset_DT$POS), max(subset_DT$POS))
        )
    }
    # ggbio::tracks(list("XGR"=xgr.track))
    if (show_plot) print(xgr.track)
    if (as.ggplot) {
        return(xgr.track@ggplot)
    } else {
        return(xgr.track)
    }
}
