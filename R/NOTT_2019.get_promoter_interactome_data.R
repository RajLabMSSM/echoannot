#' Get cell type-specific promoter/emhancer/interactome data
#'
#' Brain cell-specific epigenomic data from Nott et al. (2019).
#' @keywords internal
#' @family NOTT_2019
#' @source
#' \href{https://science.sciencemag.org/content/366/6469/1134}{Nott et al. (2019)}
#' \url{https://genome.ucsc.edu/cgi-bin/hgTracks?db=hg19&lastVirtModeType=default&lastVirtModeExtraState=&virtModeType=default&virtMode=0&nonVirtPosition=&position=chr2:127770344-127983251&hgsid=778249165_ySowqECRKNxURRn6bafH0yewAiuf}
#' @importFrom dplyr %>% rename
NOTT_2019.get_promoter_interactome_data <- function(finemap_dat) {
    Chr <- Start <- End <- chr <- NULL
    # Subset to window
    annot_sub <-
        echoannot::NOTT_2019.interactome$H3K4me3_around_TSS_annotated_pe %>%
        dplyr::rename(chr = Chr, start = Start, end = End) %>%
        subset(chr == paste0("chr", unique(finemap_dat$CHR)) &
            start >= min(finemap_dat$POS) &
            end <= max(finemap_dat$POS))
    return(annot_sub)
}
