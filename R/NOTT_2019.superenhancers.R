



#' Get cell type-specific superenhancer data
#'
#' Brain cell-specific epigenomic data from Nott et al. (2019).
#' @keywords internal
#' @family NOTT_2019
#' @source
#' \href{https://science.sciencemag.org/content/366/6469/1134}{Nott et al. (2019)}
#' \url{https://genome.ucsc.edu/cgi-bin/hgTracks?db=hg19&lastVirtModeType=default&lastVirtModeExtraState=&virtModeType=default&virtMode=0&nonVirtPosition=&position=chr2:127770344-127983251&hgsid=778249165_ySowqECRKNxURRn6bafH0yewAiuf}
#' @importFrom data.table data.table merge.data.table
#' @importFrom dplyr %>% mutate
NOTT_2019.superenhancers <- function(finemap_dat) {
    chr <- CHR <- POS <- s6 <- NULL

    annot_sub <- subset(
        echoannot::NOTT_2019.superenhancer_interactome,
        chr == paste0("chr", unique(finemap_dat$CHR)) &
            start >= min(finemap_dat$POS) &
            end <= max(finemap_dat$POS)
    )
    if (nrow(annot_sub) > 0) {
        merged_dat <- data.table:::merge.data.table(
            finemap_dat %>%
                dplyr::mutate(
                    chr = paste0("chr", CHR),
                    start = as.numeric(POS)
                ) %>%
                data.table::data.table(),
            data.table::data.table(s6),
            by = c("chr", "start")
        )
    }
    return(merged_dat)
}
