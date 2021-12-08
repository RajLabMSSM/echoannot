#' Filter sources
#'
#' Identify the sources with the most annotations in the locus.
#' Then only keep these sources.
#'
#' @param n_top_sources Number of top sources to include per library.
#' @inheritParams XGR_plot_peaks
#' @export
#' @family XGR
#' @importFrom dplyr %>% n_distinct group_by tally
#' @examples
#' gr.filt <- echoannot::XGR_filter_sources(gr.lib = echoannot::xgr_query)
XGR_filter_sources <- function(gr.lib,
                               n_top_sources = 5) {
    Source <- NULL;
    top_sources <- data.frame(gr.lib) %>%
        dplyr::group_by(library, Source) %>%
        dplyr::tally(sort = TRUE)
    if (!is.null(n_top_sources)) {
        gr.filt <- subset(
            gr.lib,
            Source %in%
                unique(top_sources$Source[
                    seq(1, min(
                        n_top_sources,
                        dplyr::n_distinct(top_sources$Source)
                    ))
                ])
        )
    }
    return(gr.filt)
}
