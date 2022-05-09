#' Import bigwig files from the UCSC Genome Browser
#' 
#' Import, preprocess, merge, and save data from 
#' \href{https://genome.ucsc.edu/}{UCSC Genome Browser} tracks.
#' 
#' @param query_dat 
#'  \link[data.table]{data.table} or
#'  \link[GenomicRanges]{GRanges}
#' containing genomic coordinates to query the UCSC tracks with.
#' @param bigwig_metadata Metadata table with at 
#' least the following two columns:
#' \itemize{
#' \item{"name"}{Unique name of the file.}
#' \item{"data_link"}{URL to UCSC genome browser bigwig file.}
#' }
#' @param save_path Where to save the processed data.
#' @param full_data Whether to download the full data 
#' (genomic ranges of all sequence reads) as opposed
#' to a reduced representation of the data as a single vector 
#' (i.e. the aggregated reads "score"). 
#' Setting \code{full_data=TRUE} is necessary for creating
#'  histograms and density plots.
#' @param xlims Min/max positions to filter from data after initial query.
#' This helps to capture genomic ranges that only partially overlap with 
#' \code{query_dat}. 
#' @param force_new If a file already exists, download a new one anyway.
#' @param nThread Number of threads to parallelise downloads across.
#' @param verbose Print messages.
#' 
#' @export
#' @importFrom echodata dt_to_granges
#' @importFrom echotabix construct_query
#' @importFrom parallel mclapply
#' @importFrom GenomicRanges GRangesList seqnames start end mcols 
#' @importFrom DescTools StrCap
#' @examples 
#' bigwig_metadata <- echoannot::NOTT2019_bigwig_metadata[1,]
#' query_dat = echodata::BST1
#' 
#' bw.gr <- echoannot::import_ucsc_bigwigs(query_dat = query_dat, 
#'                                         bigwig_metadata = bigwig_metadata)
import_ucsc_bigwigs <- function(query_dat,
                                bigwig_metadata, 
                                full_data = TRUE,
                                xlims = NULL,
                                save_path = tempfile(),
                                force_new = FALSE,
                                nThread = 1,
                                verbose = TRUE){  
    ## No longer used 
    bigwig_dir <- NULL
    #### Convert fine-map data to granges #### 
    # ! IMPORTANT !: Needs to be in chr1 format in order to query! 
    gr.query_dat <- echotabix::construct_query(query_dat = query_dat, 
                                               as_blocks = FALSE,
                                               style = "UCSC",
                                               verbose = verbose) 
    #### Check if file already exists ####
    if(!is.null(save_path) && 
       file.exists(save_path) && 
       isFALSE(force_new)){
        messager("Importing previously downloaded file:",save_path,
                 v=verbose)
        bw.filt <- readRDS(save_path)
    } else {
        messager("Downloading data from UCSC.",v=verbose) 
        #### Check metadata has necessary column ####
        check_bigwig_metadata(bigwig_metadata = bigwig_metadata)
        #### Download ####
        bw.grlist <- parallel::mclapply(
            seq_len(nrow(bigwig_metadata)),
            function(i) {
                #### Check if bigwig_dir supplied to import local files ####
                ## Otherwise, import from remote server
                if (!is.null(bigwig_dir)) {
                    bw.file <- file.path(bigwig_dir, paste0(
                        bigwig_metadata$long_name[i],
                        ".ucsc.bigWig"
                    ))
                } else {
                    bw.file <- bigwig_metadata$data_link[i]
                }
                #### Prepare bw name ####
                bw.name <- gsub("_pooled|pooled_", "", 
                                bigwig_metadata$name[i])
                messager("Importing...", 
                         paste0("[", i, "]"), bw.name,
                         v=verbose)
                bw.filt <- import_bigwig_filtered(
                    bw.file = bw.file,
                    gr.query_dat = gr.query_dat,
                    full_data = full_data
                )
                #### Processs Nott2019-specific columns ####
                for(x in c("cell_type","assay")){
                    if(x %in% colnames(bigwig_metadata)){
                        newcol <- DescTools::StrCap(x)
                        GenomicRanges::mcols(bw.filt)[newcol] <-
                            bigwig_metadata[[x]][i]
                    }
                }
                GenomicRanges::mcols(bw.filt)["Experiment"] <- 
                    gsub("_", " ", bw.name)
                return(bw.filt)
            }, mc.cores = nThread) #### END MCLAPPLY 
        bw.cols <- bigwig_metadata$name
        # names(bw.grlist) <- bw.cols
        bw.gr <- unlist(GenomicRanges::GRangesList(bw.grlist))
        #### Define x_limits ####
        ## Useful for making zoomed locus plots
        if(!is.null(xlims)){
            bw.gr <- subset(
                bw.gr,
                GenomicRanges::seqnames(bw.gr) ==
                    paste0("chr", gsub("chr", "", query_dat$CHR[1])) &
                    GenomicRanges::start(bw.gr) >= xlims[1] &
                    GenomicRanges::end(bw.gr) <= xlims[2]
            )
        } 
        #### Save data ####
        if (!is.null(save_path)) {
            messager("Saving bigwig query ==>",save_path,
                     v=verbose)
            saveRDS(bw.gr, save_path)
        } 
    } ### END DOWNLOAD 
    
    #### Return ####
    return(bw.gr)
}
