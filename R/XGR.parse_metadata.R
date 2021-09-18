#' XGR.parse_metadata
#'
#' @keywords internal
#' @family XGR
XGR.parse_metadata <- function(gr.lib,
                               lib.name = NA) {
    # https://stackoverflow.com/questions/50518137/separate-a-column-into-2-columns-at-the-last-underscore-in-r
    sep <- XGR.sep_handler(lib.name = lib.name)
    GenomicRanges::mcols(gr.lib) <- 
        tidyr::separate(data.frame(GenomicRanges::mcols(gr.lib)),
        sep = sep,
        col = "fullname",
        into = c("Source", "Assay"),
        extra = "merge"
    )
    return(gr.lib)
}
