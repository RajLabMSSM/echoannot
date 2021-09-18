#' Clean GRanges object
#'
#' @keywords internal
#' @importFrom GenomicRanges elementMetadata
clean_granges <- function(gr) {
    no_no_cols <- c(
        "seqnames", "ranges", "strand", "seqlevels", "seqlengths",
        "isCircular", "start", "end", "width", "element"
    )
    metadat <- GenomicRanges::elementMetadata(gr)
    GenomicRanges::elementMetadata(gr) <-
        metadat[, !colnames(metadat) %in% no_no_cols]
    return(gr)
}
