#' Convert data.table to GRanges object
#'
#' @family utils
#' @keywords internal
#' @importFrom GenomicRanges makeGRangesFromDataFrame
#' @importFrom GenomeInfoDb seqlevelsStyle
dt_to_granges <- function(subset_DT,
                          chrom_col="CHR",
                          start_col="POS", 
                          end_col=start_col,
                          style="NCBI",
                          verbose=TRUE){
    if (is_granges(subset_DT)) {
        messager("subset_DT is already a GRanges object.",v=verbose)
        gr.snp <- subset_DT 
    } else {
        messager("Converting subset_DT to GRanges object.",v=verbose)
        subset_DT[["SEQnames"]] <- subset_DT[[chrom_col]]
        gr.snp <- GenomicRanges::makeGRangesFromDataFrame(
            subset_DT,
              seqnames.field = "SEQnames",
              start.field = start_col,
              end.field = end_col, 
              keep.extra.columns = TRUE) 
    } 
    suppressMessages(suppressWarnings(
        GenomeInfoDb::seqlevelsStyle(gr.snp) <- style))
    return(gr.snp)
}
