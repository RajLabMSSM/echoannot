#' Get cell type-specific promoter/emhancer/interactome data
#'
#' Brain cell-specific epigenomic data from Nott et al. (2019).
#' @param dat data.table of genomic coordinates to query with.
#' Set as \code{NULL} to return genome-wide data.
#' 
#' @export
#' @family NOTT2019
#' @source
#' \href{https://doi.org/10.1126/science.aay0793}{Nott et al. (2019)}
#'
#' @importFrom dplyr rename
#' @examples 
#' dat <- echodata::BST1
#' annot_sub <- NOTT2019_get_promoter_interactome_data(dat=dat)
NOTT2019_get_promoter_interactome_data <- function(dat=NULL) {
    Chr <- Start <- End <- chr <- NULL
    # Subset to window
    NOTT2019_interactome <- get_NOTT2019_interactome()
    if(is.null(dat)) return(NOTT2019_interactome)
    annot_sub <-
        NOTT2019_interactome$H3K4me3_around_TSS_annotated_pe |>
        dplyr::rename(chr = Chr,
                      start = Start,
                      end = End) |>
        subset(chr == paste0("chr", unique(dat$CHR)) &
            start >= min(dat$POS) &
            end <= max(dat$POS))
    return(annot_sub)
}
