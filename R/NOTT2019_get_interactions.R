#' Import cell type-specific interactomes
#'
#' Brain cell-specific epigenomic data from Nott et al. (2019).
#' @keywords internal
#' @importFrom GenomicRanges makeGRangesFromDataFrame
#' @family NOTT2019
#' @source
#' \href{https://science.sciencemag.org/content/366/6469/1134}{Nott et al. (2019)}
#' \url{https://genome.ucsc.edu/cgi-bin/hgTracks?db=hg19&lastVirtModeType=default&lastVirtModeExtraState=&virtModeType=default&virtMode=0&nonVirtPosition=&position=chr2:127770344-127983251&hgsid=778249165_ySowqECRKNxURRn6bafH0yewAiuf}
NOTT2019_get_interactions <- function(dat,
                                      as.granges = FALSE,
                                      verbose = TRUE) {
    Name <- NULL;
    messager("++ NOTT2019:: Getting interaction anchors data.",v=verbose)
    NOTT2019_interactome <- get_NOTT2019_interactome()
    selected_sheets <- grep("interactome$", names(NOTT2019_interactome),
        value = TRUE
    )
    #### Import data ####
    interactomes <- lapply(selected_sheets, function(s) {
        messager("Importing", s, "...")
        # Read the sheet you want
        dat <- NOTT2019_interactome[[s]]
        dat$Name <- s
        return(dat)
    }) %>% data.table::rbindlist()
    
    #### Anchor 1 ####
    interactomes.anchor1 <- granges_overlap(
        dat1 = dat,
        chrom_col.1 = "CHR",
        start_col.1 = "POS",
        end_col.1 = "POS",
        dat2 = interactomes,
        chrom_col.2 = "chr1",
        start_col.2 = "start1",
        end_col.2 = "end1"
    )
    GenomicRanges::mcols(interactomes.anchor1)["Anchor"] <- 1
    #### Anchor 2 ####
    interactomes.anchor2 <- granges_overlap(
        dat1 = dat,
        chrom_col.1 = "CHR",
        start_col.1 = "POS",
        end_col.1 = "POS",
        dat2 = interactomes,
        chrom_col.2 = "chr2",
        start_col.2 = "start2",
        end_col.2 = "end2"
    )
    GenomicRanges::mcols(interactomes.anchor2)["Anchor"] <- 2
    #### Merge ####
    interactomes.anchor <- c(
        interactomes.anchor1,
        interactomes.anchor2
    )
    #### Modify ####
    GenomicRanges::mcols(interactomes.anchor)["Assay"] <- "PLAC"
    interactomes.anchor <- cbind(
        data.frame(interactomes.anchor),
        tidyr::separate(data.frame(interactomes.anchor, 
                                   check.names = FALSE),
            Name,
            into = c("Cell_type", "Element")
        )[, c("Cell_type", "Element")]
    )
    cell_dict <- c(
        "Microglia" = "microglia",
        "Neuronal" = "neurons",
        "Oligo" = "oligo"
    )
    interactomes.anchor$Cell_type <- cell_dict[interactomes.anchor$Cell_type]

    if (as.granges) {
        interactomes.anchor <- echodata::dt_to_granges(
            dat = interactomes.anchor %>% dplyr::select(-seqnames),
            chrom_col = "CHR",
            start_col = "start",
            end_col = "end",
            style = "NCBI",
            verbose = verbose
        ) 
    }
    return(interactomes.anchor)
}
