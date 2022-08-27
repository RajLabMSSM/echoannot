#' MOTIFBREAKR: make ID
#' 
#' MOTIFBREAKR support function.
#' @inheritParams MOTIFBREAKR_filter
#' @keywords internal
#' @returns Motif disruption predictions in 
#'  \link[GenomicRanges]{GRanges} format.
MOTIFBREAKR_make_id <- function(mb_res){
    mb_res$id <- paste(mb_res$SNP_id,
                       mb_res$dataSource, 
                       mb_res$providerId, 
                       sep='_')
    return(mb_res)
}
