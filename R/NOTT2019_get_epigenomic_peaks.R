#' Download cell type-specific epigenomic peaks
#'
#' API access to brain cell type-specific epigenomic peaks (bed format)
#' from Nott et al. (2019).
#' 
#' @param assays Which epigenomic assays to import data from.
#' @param cell_type Which cell-type to import data from.
#' @param convert_to_granges Whether to convert the peaks to a 
#' \link[GenomicRanges]{GRanges} object.
#' 
#' @param save_dir Where to save the processed data.
#' @param force_new If the saved data already exists, re-downloaded anyway.
#' @param nThread Number of threads to parallelise downloads across.
#' @param verbose Print messages.
#' 
#' @family NOTT2019
#' @source
#' \href{https://science.sciencemag.org/content/366/6469/1134}{Nott et al. (2019)}
#' \url{https://genome.ucsc.edu/cgi-bin/hgTracks?db=hg19&lastVirtModeType=default&lastVirtModeExtraState=&virtModeType=default&virtMode=0&nonVirtPosition=&position=chr2:127770344-127983251&hgsid=778249165_ySowqECRKNxURRn6bafH0yewAiuf}
#' 
#' @export
#' @importFrom dplyr %>%
#' @importFrom data.table fread
#' @importFrom parallel mclapply
#' @importFrom echodata dt_to_granges
#' @examples
#' PEAKS <- echoannot::NOTT2019_get_epigenomic_peaks() 
NOTT2019_get_epigenomic_peaks <- function(assays = c(
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
                                          convert_to_granges = TRUE,
                                          save_dir = tempdir(),
                                          force_new = FALSE,
                                          nThread = 1,
                                          verbose = TRUE) {
    #### Prepare paths ####
    baseURL <- file.path(
        "https://raw.githubusercontent.com/nottalexi",
        "brain-cell-type-peak-files/master"
    )
    local_path <- file.path(save_dir, "NOTT2019_epigenomic_peaks.rds")
    #### Download or import existing data ####
    if(file.exists(local_path) & force_new==FALSE){
        messager("Importing previously downloaded files:",local_path,
                 v=verbose)
        PEAKS <- readRDS(local_path)
    } else {
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
        
        messager("++ NOTT2019:: Downloading and merging",
                 length(file_names), "peaks BED files.",
                 v = verbose
        )
        PEAKS <- parallel::mclapply(file_names,
                                    function(f, .verbose = verbose) {
            messager("++ NOTT2019:: Downloading", f, v = .verbose)
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
        
        if (convert_to_granges) { 
            PEAKS <- echodata::dt_to_granges(dat = PEAKS,
                                      chrom_col = "chr", 
                                      start_col = "start", 
                                      end_col = "end", 
                                      verbose = verbose) 
        }
        #### Save ####
        if(!is.null(save_dir)){
            messager("Saving data ==>",local_path,v=verbose)
            dir.create(dirname(local_path), 
                       showWarnings = FALSE, recursive = TRUE)
            saveRDS(PEAKS, file = local_path)
        } 
    }
    #### Report #####
    messager("++ NOTT2019::", formatC(length(PEAKS),big.mark = ","),
             "ranges retrieved.", v = verbose)
    return(PEAKS)
}
