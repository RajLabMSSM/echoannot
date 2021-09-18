#' Import filtered bigwig
#'
#' Import a subset of a bigwig file
#' based on the coordinates in a GRanges object (gr.dat).
#' @param bw.file Path to a bigwig file.
#' @param gr.dat GenomicRanges object to query the bigwig file with.
#' @param full_data Whether to return the actual read ranges (\code{full_data=T}),
#' or just the "score" column which summarizes the height of
#' the aggregated reads across the genome (\code{full_data=TRUE}).
#' @keywords internal
#' @importFrom GenomicRanges mcols start end
#' @importFrom rtracklayer import.bw
import.bw.filt <- function(bw.file,
                           gr.dat,
                           full_data = TRUE) {
    if (full_data) {
        # Get all ranges within min/max
        gr.span <- gr.dat[1, ]
        GenomicRanges::mcols(gr.span) <- NULL
        GenomicRanges::start(gr.span) <-
            min(GenomicRanges::start(gr.dat), na.rm = TRUE)
        GenomicRanges::end(gr.span) <-
            max(GenomicRanges::end(gr.dat), na.rm = TRUE)
    } else {
        # Otherwise, just use the score for the exact values
        gr.span <- gr.dat
    }
    # bw.dat <- rtracklayer::BigWigSelection(ranges = gr.dat,
    # colnames = "score")
    bw.filt <- rtracklayer::import.bw(
        con = bw.file,
        selection = gr.span
    )
    # plot(x = start(bw.filt), y=bw.filt$score)
    return(bw.filt)
}
