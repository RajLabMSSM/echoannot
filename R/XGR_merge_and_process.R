#' Standardize XGR annotations
#'
#' Parses the metadata and adds it as columns,
#' and then merges the results into a single
#' \code{\link[GenomicRanges]{GenomicRangesList}}
#'
#' @param grl.xgr \link[GenomicRanges]{GenomicRangesList} of XGR queries.
#' @family XGR
#' @keywords internal
#' @importFrom GenomicRanges start end
#' @importFrom dplyr group_by tally
XGR_merge_and_process <- function(grl.xgr,
                                  lib,
                                  n_top_sources = 10) {
    
    
    Source <- NULL;
    # grl.xgr <- check_saved_XGR(results_path, lib)
    ## Make track
    ## Add and modify columns
    grl.XGR_merged <- unlist(grl.xgr)
    names(grl.XGR_merged) <- gsub("Broad_Histone_", "", names(grl.XGR_merged))
    sep <- XGR_sep_handler(lib.name = lib)
    grl.XGR_merged$Source <- lapply(
        names(grl.XGR_merged),
        function(e) {
            strsplit(e, sep)[[1]][1]
        }
    ) |>
        unlist()
    # grl.XGR_merged$Source <- gsub("_","\n", grl.XGR_merged$Source)
    grl.XGR_merged$Assay <- lapply(
        names(grl.XGR_merged),
        function(e) {
            strsplit(e, sep)[[1]][2]
        }
    ) |>
        unlist()
    grl.XGR_merged$Start <- GenomicRanges::start(grl.XGR_merged)
    grl.XGR_merged$End <- GenomicRanges::end(grl.XGR_merged)
    # Filter
    top_sources <- grl.XGR_merged |>
        data.frame() |>
        dplyr::group_by(Source) |>
        dplyr::tally(sort = T)
    grl.XGR_merged.filt <- subset(
        grl.XGR_merged,
        Source %in% unique(top_sources$Source[seq(1, n_top_sources)])
    )
    # Count
    # snp.pos <- subset(gr.snp, SNP %in% c("rs7294619"))$POS
    # snp.sub <- subset(grl.XGR_merged,
    # Start<=snp.pos & End>=snp.pos) |> data.frame()
    grl.XGR_merged.filt$Source_Assay <- paste0(
        grl.XGR_merged.filt$Source,
        "_", grl.XGR_merged.filt$Assay
    )
    return(grl.XGR_merged.filt)
}
