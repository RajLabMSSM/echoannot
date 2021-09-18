#' Plot brain cell-specific epigenomic data
#'
#' @keywords internal
#' @family NOTT_2019
#' @source
#' \href{https://science.sciencemag.org/content/366/6469/1134}{Nott et al. (2019)}
#' \url{https://genome.ucsc.edu/cgi-bin/hgTracks?db=hg19&lastVirtModeType=default&lastVirtModeExtraState=&virtModeType=default&virtMode=0&nonVirtPosition=&position=chr2:127770344-127983251&hgsid=778249165_ySowqECRKNxURRn6bafH0yewAiuf}
#' @importFrom GenomicRanges makeGRangesFromDataFrame
#' @importFrom parallel mclapply
#' @importFrom data.table rbindlist
#' @importFrom GenomeInfoDb seqlevelsStyle
NOTT_2019.get_regulatory_regions <- function(as.granges = FALSE,
                                             nThread = 1,
                                             verbose = TRUE) {
    Name <- Cell_type <- NULL

    selected_sheets <- grep("promoters$|enhancers$",
        names(echoannot::NOTT_2019.interactome),
        value = TRUE
    )
    regions <- parallel::mclapply(selected_sheets, function(s) {
        messager("Importing", s, "...")
        dat <- echoannot::NOTT_2019.interactome[[s]]
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
        tidyr::separate(Name, into = c("Cell_type", "Element"), remove = FALSE) %>%
        dplyr::mutate(
            middle = as.integer(end - abs(end - start) / 2),
            Cell_type = cell_dict[Cell_type]
        )
    if (as.granges) {
        messager("++ Converting to GRanges.", v = verbose)
        regions_sub <- GenomicRanges::makeGRangesFromDataFrame(
            df = regions_sub,
            # dplyr::mutate(regions_sub, chr = as.numeric(gsub("chr","",chr))),
            seqnames.field = "chr",
            start.field = "start",
            end.field = "end",
            keep.extra.columns = TRUE
        )
        suppressWarnings(GenomeInfoDb::seqlevelsStyle(regions_sub) <- "NCBI")
    }
    return(regions_sub)
}
