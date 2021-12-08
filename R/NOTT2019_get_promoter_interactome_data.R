#' Get cell type-specific promoter/emhancer/interactome data
#'
#' Brain cell-specific epigenomic data from Nott et al. (2019).
#' @keywords internal
#' @family NOTT2019
#' @source
#' \href{https://science.sciencemag.org/content/366/6469/1134}{Nott et al. (2019)}
#' \url{https://genome.ucsc.edu/cgi-bin/hgTracks?db=hg19&lastVirtModeType=default&lastVirtModeExtraState=&virtModeType=default&virtMode=0&nonVirtPosition=&position=chr2:127770344-127983251&hgsid=778249165_ySowqECRKNxURRn6bafH0yewAiuf}
#' @importFrom dplyr %>% rename
NOTT2019_get_promoter_interactome_data <- function(dat) {
    Chr <- Start <- End <- chr <- NULL
    # Subset to window
    NOTT2019_interactome <- get_NOTT2019_interactome()
    annot_sub <-
        NOTT2019_interactome$H3K4me3_around_TSS_annotated_pe %>%
        dplyr::rename(chr = Chr, start = Start, end = End) %>%
        subset(chr == paste0("chr", unique(dat$CHR)) &
            start >= min(dat$POS) &
            end <= max(dat$POS))
    return(annot_sub)
}
