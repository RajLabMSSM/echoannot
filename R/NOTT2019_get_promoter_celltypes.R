#' Get promoter cell types
#'
#' Brain cell-specific epigenomic data from Nott et al. (2019).
#' @keywords internal
#' @family NOTT2019
NOTT2019_get_promoter_celltypes <- function(annot_sub,
                                            marker_key) {
    promoter.cols <- grep("*_active_promoter",
        colnames(annot_sub),
        value = TRUE
    )
    logical.list <- colSums(annot_sub[, promoter.cols]) > 0
    promoter_celltypes <- gsub(
        "\\_.*", "",
        promoter.cols[as.logical(logical.list)]
    )
    promoter_celltypes <- as.character(marker_key[promoter_celltypes])
    promoter_celltypes <- paste(promoter_celltypes, collapse = "; ")
    return(promoter_celltypes)
}
