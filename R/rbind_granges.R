#' Bind GRanges with different mcols
#'
#' @family utils
#' #' @examples
#' \dontrun{
#' merged_DT <- echodata::get_Nalls2019_merged()
#' gr.hits <- CORCES2020_get_ATAC_peak_overlap(dat = merged_DT)
#' gr.hits$extra_col <- "Extra"
#' gr.anchor_hits <- CORCES2020_get_hichip_fithichip_overlap(
#'     dat = merged_DT)
#' try({gr.bind <- c(gr.hits, gr.anchor_hits)})
#' gr.bound <- rbind_granges(gr1, gr2)
#' }
#' @keywords internal
#' @importFrom GenomicRanges mcols
rbind_granges <- function(gr1, gr2) {
    gr1_not_gr2 <- colnames(
        GenomicRanges::mcols(gr1)
    )[
        !colnames(GenomicRanges::mcols(gr1)) %in%
            colnames(GenomicRanges::mcols(gr2))
    ]
    gr2_not_gr1 <- colnames(
        GenomicRanges::mcols(gr2)
    )[
        !colnames(GenomicRanges::mcols(gr2)) %in%
            colnames(GenomicRanges::mcols(gr1))
    ]
    gr1 <- gr1[, !colnames(GenomicRanges::mcols(gr1)) %in%
        c(gr1_not_gr2, gr2_not_gr1)]
    gr2 <- gr2[, !colnames(GenomicRanges::mcols(gr2)) %in%
        c(gr1_not_gr2, gr2_not_gr1)]
    gr.bound <- c(gr1, gr2)
    return(gr.bound)
}
