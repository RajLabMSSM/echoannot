#' Filter sources
#'
#' Identify the sources with the most annotations in the locus.
#' Then only keep these sources.
#' @keywords internal
#' @family XGR
#' @importFrom dplyr n_distinct
XGR.filter_sources <- function(gr.lib,
                               n_top_sources = 5) {
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
