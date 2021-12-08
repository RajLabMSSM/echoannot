#' name_filter_convert
#'
#' @family plot
#' @keywords internal
name_filter_convert <- function(GR.final,
                                GR.names,
                                min_hits = 1) {
    names(GR.final) <- GR.names
    grl <- GR.final[!as.logical(lapply(GR.final, is.null))]
    # Filter to those that had at least N hits
    grl <- grl[as.logical(lapply(grl, function(g, min_hits. = min_hits) {
        length(GenomicRanges::seqnames(g)) >= min_hits.
    }))]
    # Convert to GRangesList (important)
    grl <- GenomicRanges::GRangesList(grl)
    return(grl)
}
