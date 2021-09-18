#' Find overlap between genomic coordinates/ranges
#'
#' @family GRanges
#' @keywords internal
#' @importFrom GenomicRanges makeGRangesFromDataFrame mcols findOverlaps
#' @importFrom GenomeInfoDb seqlevelsStyle
#' @importFrom S4Vectors queryHits subjectHits
granges_overlap <- function(dat1,
                            dat2,
                            chrom_col.1 = "chrom",
                            start_col.1 = "start",
                            end_col.1 = "end",
                            chrom_col.2 = chrom_col.1,
                            start_col.2 = start_col.1,
                            end_col.2 = end_col.1,
                            return_merged = TRUE,
                            chr_format = "NCBI",
                            verbose = FALSE) {
    # dat1
    if (is_granges(dat1)) {
        messager("+ dat1 already in GRanges format", v = verbose)
        gr.dat1 <- dat1
    } else {
        gr.dat1 <- GenomicRanges::makeGRangesFromDataFrame(
            dat1,
            seqnames.field = chrom_col.1,
            start.field = start_col.1,
            end.field = end_col.1,
            ignore.strand = TRUE,
            keep.extra.columns = TRUE
        )
    }
    # dat2
    if (is_granges(dat2)) {
        messager("+ dat2 already in GRanges format", v = verbose)
        gr.dat2 <- dat2
    } else {
        messager("+ Converting dat2 to GRanges", v = verbose)
        gr.dat2 <- GenomicRanges::makeGRangesFromDataFrame(
            dat2,
            seqnames.field = chrom_col.2,
            start.field = start_col.2,
            end.field = end_col.2,
            ignore.strand = TRUE,
            keep.extra.columns = TRUE
        )
    }
    # Standardize seqnames format
    suppressWarnings(GenomeInfoDb::seqlevelsStyle(gr.dat1) <- chr_format)
    suppressWarnings(GenomeInfoDb::seqlevelsStyle(gr.dat2) <- chr_format)
    hits <- GenomicRanges::findOverlaps(
        query = gr.dat1,
        subject = gr.dat2
    )
    gr.hits <- gr.dat2[S4Vectors::subjectHits(hits), ]
    if (return_merged) {
        messager("+ Merging both GRanges.", v = verbose)
        GenomicRanges::mcols(gr.hits) <- cbind(
            GenomicRanges::mcols(gr.hits),
            GenomicRanges::mcols(gr.dat1[S4Vectors::queryHits(hits), ])
        )
    }

    # gr.hits <- cbind(mcols(gr.regions[ S4Vectors::subjectHits(hits), ] ),
    #                         mcols(gr.consensus[S4Vectors::queryHits(hits),]) )
    message(
        "", formatC(nrow(GenomicRanges::mcols(gr.hits)), big.mark = ","),
        " query SNP(s) detected with reference overlap."
    )
    # print(data.frame(mcols(gr.hits[,c("Name","SNP")])) )
    suppressWarnings(GenomeInfoDb::seqlevelsStyle(gr.hits) <- chr_format)
    return(gr.hits)
}
