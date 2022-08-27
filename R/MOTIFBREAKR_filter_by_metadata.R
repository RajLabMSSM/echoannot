#' Filter \pkg{motifbreakR} results
#' 
#' Filter by motif database metadata.
#' @inheritParams MOTIFBREAKR
#' @inheritParams MOTIFBREAKR_filter
#' @family motifbreakR
#' @keywords internal
#' @importFrom GenomicRanges mcols
#' @importFrom BiocGenerics %in%
MOTIFBREAKR_filter_by_metadata <- function(mb_res,
                                           Organism="Hsapiens"){
    requireNamespace("MotifDb")
    organism <- providerId <- NULL;
    meta <- subset(GenomicRanges::mcols(MotifDb::MotifDb), organism==Organism)
    mb.filtered <-subset(mb_res, providerId %in% meta$providerId)
    return(mb.filtered)
}
