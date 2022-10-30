#' Filter assays
#'
#' Identify the assays with the most annotations in the locus.
#' Then only keep these assays.
#' @param gr.lib Results from \link[echoannot]{XGR_query}.
#' @param n_top_assays Number of top assays to include per library. 
#' @export
#' @family XGR
#' @importFrom dplyr n_distinct group_by tally
#' @examples
#' gr.filt <- echoannot::XGR_filter_assays(gr.lib = echoannot::xgr_example)
XGR_filter_assays <- function(gr.lib,
                              n_top_assays = 1) {
    Assay <- NULL;
    top_assays <- data.frame(gr.lib) |>
        dplyr::group_by(library, Assay) |>
        dplyr::tally(sort = TRUE)
    if (!is.null(n_top_assays)) {
        gr.lib <- subset(
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
    return(gr.lib)
}
