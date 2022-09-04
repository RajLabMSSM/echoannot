#' name_filter_convert
#'
#' @family plot
#' @keywords internal
#' @importFrom BiocGenerics %in% 
name_filter_convert <- function(grl,
                                min_hits = 1) {
    
    grl <- grl[!as.logical(lapply(grl, is.null))]
    # Filter to those that had at least N hits
    grl <- grl[as.logical(lapply(grl, function(g, min_hits. = min_hits) {
        length(GenomicRanges::seqnames(g)) >= min_hits.
    }))] 
    #### Convert to GRangesList (important) ####
    grl <- GenomicRanges::GRangesList(grl)
    return(grl)
}
