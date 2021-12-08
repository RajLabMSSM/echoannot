#' Check overlap with XGR annotations
#'
#' Automatically handles different file formats provided by XGR
#'  (e.g. varying kinds of nested/unnested \code{GRanges}).
#' Then returns a \code{Granges} object with only the XGR annotation ranges
#' that overlap with the SNPs in \code{dat}.
#' The \code{GRanges} merges hits from \code{dat}.
#'
#' @param nThread Number of threads to parallelise across libraries.
#' @param save_path Save the results as a \code{data.frame}.
#' @inheritParams XGR_prepare_foreground_background
#' @inheritParams XGR_download_and_standardize
#' @family XGR
#' @keywords internal
#' @examples
#' \dontrun{
#' gr.hits <- XGR_iterate_overlap(
#'     lib.selections = c("ENCODE_TFBS_ClusteredV3_CellTypes"),
#'     dat = echodata::BST1
#' )
#' }
#' @importFrom data.table fwrite
XGR_iterate_overlap <- function(lib.selections =
                                    c(
                                        "ENCODE_TFBS_ClusteredV3_CellTypes",
                                        "TFBS_Conserved",
                                        "ReMap_PublicAndEncode_TFBS",
                                        "Uniform_TFBS"
                                    ),
                                dat,
                                save_path = FALSE,
                                nThread = 1) {
    
    Consensus_SNP <- NULL;
    gr.lib <- XGR_download_and_standardize(
        lib.selections = lib.selections,
        dat = dat,
        nThread = nThread
    )
    gr.hits <- granges_overlap(
        dat1 = dat,
        chrom_col.1 = "CHR",
        start_col.1 = "POS",
        end_col.1 = "POS",
        dat2 = gr.lib
    )

    ucs.hits <- subset(gr.hits, Consensus_SNP)
    length(unique(subset(dat, Consensus_SNP)$SNP))
    length(unique(subset(dat, Consensus_SNP)$Locus))

    length(unique(ucs.hits$SNP))
    length(unique(ucs.hits$Locus))

    if (save_path != FALSE) {
        dir.create(dirname(save_path),
            showWarnings = FALSE, recursive = TRUE
        )
        data.table::fwrite(data.frame(ucs.hits), save_path)
    }
    return(gr.hits)
}
