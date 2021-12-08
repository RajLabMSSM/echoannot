#' Find GenomicRanges overlap
#'  
#' Find overlap genomic position overlap between two
#' \link[GenomicRanges]{GRanges} objects.
#' @param dat1 Dataset 1 
#' (can be \link[GenomicRanges]{GRanges} or \link[data.table]{data.table}).
#' @param dat2 Dataset 2.
#' (can be \link[GenomicRanges]{GRanges} or \link[data.table]{data.table}).
#' @param chrom_col.1 Name of the chromosome column in \code{dat1}.
#' @param start_col.1 Name of the start position column in \code{dat1}.
#' @param end_col.1 Name of the end position column in \code{dat2}.
#' @param chrom_col.2 Name of the chromosome column in \code{dat2}.
#' @param start_col.2 Name of the start position column in \code{dat2}.
#' @param end_col.2 Name of the end position column in \code{dat2}.
#' @param return_merged Whether to return an object with columns
#'  from \code{dat1} and \code{dat2} merged.
#' @param unique_only Only return unique rows. 
#' @inheritParams echodata::dt_to_granges
#' @family GRanges
#' @export
#' @importFrom GenomicRanges mcols findOverlaps
#' @importFrom S4Vectors queryHits subjectHits
#' @importFrom echodata dt_to_granges
#' @examples
#' dat1 <- echodata::BST1
#' dat2 <- echoannot::xgr_query
#' GenomicRanges::mcols(dat2) <- NULL
#' 
#' gr.hits <- echoannot::granges_overlap(dat1 = dat1, 
#'                                       dat2 = dat2, 
#'                                       chrom_col.1 = "CHR",
#'                                       start_col.1 = "POS")
granges_overlap <- function(dat1,
                            dat2,
                            chrom_col.1 = "chrom",
                            start_col.1 = "start",
                            end_col.1 = start_col.1,
                            chrom_col.2 = "chrom",
                            start_col.2 = "start",
                            end_col.2 = end_col.2,
                            return_merged = TRUE,
                            unique_only = TRUE,
                            style = "NCBI",
                            verbose = FALSE) {
    messager("Determing overlap between 2 GRanges objects.",v=verbose)
    #### dat1 ####
    gr.dat1 <- echodata::dt_to_granges(
        dat = dat1,
        chrom_col = chrom_col.1,
        start_col = start_col.1, 
        end_col = end_col.1,
        style = style)
    ##### dat2 ####
    gr.dat2 <- echodata::dt_to_granges(
        dat = dat2,
        chrom_col = chrom_col.2,
        start_col = start_col.2,
        end_col = end_col.2,
        style = style,
        verbose = verbose)
    #### Find overlap ####
    hits <- GenomicRanges::findOverlaps(
        query = gr.dat1,
        subject = gr.dat2
    ) 
    gr.hits <- gr.dat2[S4Vectors::subjectHits(hits), ]
    #### Merge ####
    if (return_merged) {
        messager("+ Merging both GRanges.", v = verbose)
        GenomicRanges::mcols(gr.hits) <- cbind(
            GenomicRanges::mcols(gr.hits),
            GenomicRanges::mcols(gr.dat1[S4Vectors::queryHits(hits), ])
        )
    } 
    #### Only return unique rows ####
    if(unique_only) gr.hits <- unique(gr.hits)
    #### Report ####
    message(
        "", formatC(nrow(GenomicRanges::mcols(gr.hits)), big.mark = ","),
        " query SNP(s) detected with reference overlap."
    ) 
    #### Return ####
    return(gr.hits)
}
