#' Filter assays
#'
#' Identify the assays with the most annotations in the locus.
#' Then only keep these assays
#' @keywords internal
#' @family XGR
#' @importFrom dplyr %>% n_distinct group_by tally
XGR.filter_assays <- function(gr.lib,
                              n_top_assays = 5) {
    top_assays <- data.frame(gr.lib) %>%
        dplyr::group_by(library, Assay) %>%
        dplyr::tally(sort = T)
    if (!is.null(n_top_assays)) {
        gr.filt <- subset(
            gr.lib,
            Assay %in%
                unique(top_assays$Assay[
                    seq(1, min(
                        n_top_assays,
                        dplyr::n_distinct(top_assays$Assay)
                    ))
                ])
        )
    }
    return(gr.filt)
}
