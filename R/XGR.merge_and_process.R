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
#' @importFrom dplyr %>% group_by tally
XGR.merge_and_process <- function(grl.xgr,
                                  lib,
                                  n_top_sources = 10) {
    # grl.xgr <- check_saved_XGR(results_path, lib)
    ## Make track
    ## Add and modify columns
    grl.xgr.merged <- unlist(grl.xgr)
    names(grl.xgr.merged) <- gsub("Broad_Histone_", "", names(grl.xgr.merged))
    sep <- XGR.sep_handler(lib.name = lib)
    grl.xgr.merged$Source <- lapply(
        names(grl.xgr.merged),
        function(e) {
            strsplit(e, sep)[[1]][1]
        }
    ) %>%
        unlist()
    # grl.xgr.merged$Source <- gsub("_","\n", grl.xgr.merged$Source)
    grl.xgr.merged$Assay <- lapply(
        names(grl.xgr.merged),
        function(e) {
            strsplit(e, sep)[[1]][2]
        }
    ) %>%
        unlist()
    grl.xgr.merged$Start <- GenomicRanges::start(grl.xgr.merged)
    grl.xgr.merged$End <- GenomicRanges::end(grl.xgr.merged)
    # Filter
    top_sources <- grl.xgr.merged %>%
        data.frame() %>%
        dplyr::group_by(Source) %>%
        dplyr::tally(sort = T)
    grl.xgr.merged.filt <- subset(
        grl.xgr.merged,
        Source %in% unique(top_sources$Source[seq(1, n_top_sources)])
    )
    # Count
    # snp.pos <- subset(gr.snp, SNP %in% c("rs7294619"))$POS
    # snp.sub <- subset(grl.xgr.merged,
    # Start<=snp.pos & End>=snp.pos) %>% data.frame()
    grl.xgr.merged.filt$Source_Assay <- paste0(
        grl.xgr.merged.filt$Source,
        "_", grl.xgr.merged.filt$Assay
    )
    return(grl.xgr.merged.filt)
}
