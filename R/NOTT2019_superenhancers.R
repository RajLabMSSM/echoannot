



#' Get cell type-specific superenhancer data
#'
#' Brain cell-specific epigenomic data from Nott et al. (2019).
#' @keywords internal
#' @family NOTT2019
#' @source
#' \href{https://doi.org/10.1126/science.aay0793}{Nott et al. (2019)}
#'
#' @importFrom data.table data.table merge.data.table
#' @importFrom dplyr mutate
NOTT2019_superenhancers <- function(dat) {
    chr <- CHR <- POS <- s6 <- NULL

    annot_sub <- subset(
        get_NOTT2019_superenhancer_interactome(),
        chr == paste0("chr", unique(dat$CHR)) &
            start >= min(dat$POS) &
            end <= max(dat$POS)
    )
    if (nrow(annot_sub) > 0) {
        merged_dat <- data.table::merge.data.table(
            data.table::data.table(dat) |>
                dplyr::mutate(
                    chr = paste0("chr", CHR),
                    start = as.numeric(POS)
                ) |>
                data.table::data.table(),
            data.table::data.table(s6),
            by = c("chr", "start")
        )
    }
    return(merged_dat)
}
