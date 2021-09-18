#' Download cell type-specific epigenomic peaks
#'
#' API access to brain cell type-specific epigenomic peaks (bed format)
#' from Nott et al. (2019).
#' @keywords internal
#' @family NOTT_2019
#' @source
#' \href{https://science.sciencemag.org/content/366/6469/1134}{Nott et al. (2019)}
#' \url{https://genome.ucsc.edu/cgi-bin/hgTracks?db=hg19&lastVirtModeType=default&lastVirtModeExtraState=&virtModeType=default&virtMode=0&nonVirtPosition=&position=chr2:127770344-127983251&hgsid=778249165_ySowqECRKNxURRn6bafH0yewAiuf}
#' @examples
#' PEAKS <- NOTT_2019.get_epigenomic_peaks(nThread = 1)
#' @importFrom GenomicRanges makeGRangesFromDataFrame
#' @importFrom data.table fread
#' @importFrom parallel mclapply
#' @importFrom GenomeInfoDb seqlevelsStyle
NOTT_2019.get_epigenomic_peaks <- function(assays = c(
                                               "ATAC",
                                               "H3K27ac",
                                               "H3K4me3"
                                           ),
                                           cell_types = c(
                                               "neurons",
                                               "microglia",
                                               "oligo",
                                               "astrocytes"
                                           ),
                                           convert_to_GRanges = TRUE,
                                           nThread = 1,
                                           verbose = TRUE) {
    baseURL <- file.path(
        "https://raw.githubusercontent.com/nottalexi",
        "brain-cell-type-peak-files/master"
    )
    cell_dict <- list(
        neurons = "NeuN",
        microglia = "PU1",
        oligo = "Olig2",
        astrocytes = "LHX2",
        periph = "peripheral PU1+"
    )
    cell_dict_invert <- as.list(setNames(names(cell_dict), cell_dict))
    assay_dict <- list(
        ATAC = "_optimal_peak_IDR_ENCODE.ATAC.bed",
        H3K27ac = "_optimal_peak.H3K27.bed",
        H3K4me3 = "_optimal_peak.H3K4me3.bed"
    )
    file_names <- unlist(lapply(assays, function(assay) {
        file.path(assay, paste0(cell_dict[cell_types], assay_dict[assay]))
    }))

    messager("++ NOTT_2019:: Downloading and merging",
        length(file_names), "peaks BED files.",
        v = verbose
    )
    PEAKS <- parallel::mclapply(file_names, function(f, .verbose = verbose) {
        messager("++ NOTT_2019:: Downloading", f, v = .verbose)
        bed_dat <- data.table::fread(file.path(baseURL, f),
            col.names = c("chr", "start", "end"),
            # IMPORTANT! must =1 if parallelizing
            nThread = 1
        )
        bed_dat$Assay <- dirname(f)
        bed_dat$Marker <- strsplit(basename(f), "_")[[1]][1]
        bed_dat$Cell_type <-
            cell_dict_invert[[strsplit(basename(f), "_")[[1]][1]]]
        return(bed_dat)
    }, mc.cores = nThread) %>% data.table::rbindlist()

    if (convert_to_GRanges) {
        messager("++ NOTT_2019:: Converting merged BED files to GRanges.",
            v = verbose
        )
        PEAKS <- GenomicRanges::makeGRangesFromDataFrame(PEAKS,
            seqnames.field = "chr",
            start.field = "start",
            end.field = "end",
            keep.extra.columns = TRUE
        )
        suppressWarnings(GenomeInfoDb::seqlevelsStyle(PEAKS) <- "NCBI")
    }
    messager("++ NOTT_2019::", length(PEAKS), "ranges retrieved.", v = verbose)
    return(PEAKS)
}
