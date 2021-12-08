#' Plot brain cell-specific epigenomic data
#'
#' @keywords internal
#' @family NOTT2019
#' @source
#' \href{https://science.sciencemag.org/content/366/6469/1134}{Nott et al. (2019)}
#' \url{https://genome.ucsc.edu/cgi-bin/hgTracks?db=hg19&lastVirtModeType=default&lastVirtModeExtraState=&virtModeType=default&virtMode=0&nonVirtPosition=&position=chr2:127770344-127983251&hgsid=778249165_ySowqECRKNxURRn6bafH0yewAiuf}
#' @importFrom parallel mclapply
#' @importFrom data.table rbindlist 
NOTT2019_get_regulatory_regions <- function(as.granges = FALSE,
                                            nThread = 1,
                                            verbose = TRUE) {
    Name <- Cell_type <- NULL

    NOTT2019_interactome <- get_NOTT2019_interactome()
    selected_sheets <- grep("promoters$|enhancers$",
        names(NOTT2019_interactome),
        value = TRUE
    )
    regions <- parallel::mclapply(selected_sheets, function(s) {
        messager("Importing", s, "...")
        dat <- NOTT2019_interactome[[s]]
        dat$Name <- tolower(s)
        return(dat)
    }, mc.cores = nThread) %>% data.table::rbindlist(fill = TRUE)

    cell_dict <- c(
        "astrocyte" = "astrocytes",
        "neuronal" = "neurons",
        "oligo" = "oligo",
        "oligodendrocytes" = "oligo",
        "microglia" = "microglia"
    )
    regions_sub <- regions %>%
        tidyr::separate(Name,
            into = c("Cell_type", "Element"),
            remove = FALSE
        ) %>%
        dplyr::mutate(
            middle = as.integer(end - abs(end - start) / 2),
            Cell_type = cell_dict[Cell_type]
        )
    if (as.granges) {
        regions_sub <- echodata::dt_to_granges(dat = regions_sub, 
                                               chrom_col = "chr", 
                                               start_col = "start",
                                               end_col = "end",
                                               style = "NCBI")
    }
    return(regions_sub)
}
