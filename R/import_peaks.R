#' Import peaks
#' 
#' Import pe-computed peak files, or
#' compute new peaks from bedGraph files.
#' Automatically detects which database each accession ID is from
#'  (\href{http://www.ncbi.nlm.nih.gov/geo}{GEO} or 
#'  \href{https://www.encodeproject.org/}{ENCODE})
#' and queries a subset of ranges specified in \code{query_granges}.
#' 
#' @param ids Sample IDs from GEO (e.g. "GSM4271282") 
#' or ENCODE (e.g. "ENCFF048VDO").
#' @param builds Genome build that each sample in \code{ids} is aligned to.
#' This will determine whether whether the \code{query_granges} data need to be 
#' lifted over to different genome build before querying.
#' Can be a single character string applied to all \code{ids} (e.g. "hg19"), 
#' or a vector of the same length as \code{ids} named using the \code{ids} 
#' (e.g. c("GSM4271282"="hg19", "ENCFF048VDO"="hg38")).  
#' @param query_granges [Optional]
#' \link[GenomicRanges]{GRanges} object indicating which genomic
#' regions to extract from each sample.
#' @param query_granges_build [Optional]
#' Genome build that \code{query_granges} is aligned to.
#' @param force_new By default, saved results of the same \code{save_path} name 
#' will be imported instead of running queries. However you can override this
#' by setting \code{force_new} to perform new queries regardless and overwrite
#'  the old \code{save_path} file.
#' @param peaks_dir Directory to save peaks to 
#' (only used when calling peaks from bedGraph files).
#' @param save_path Path to save query results to in \emph{.rds} format.
#' @param split_chromosomes Split single-threaded query 
#' into multi-threaded query across chromosomes. 
#' This is can be helpful especially when calling peaks from 
#' large bigWig/bedGraph files.
#' The number of threads used is set by the \code{nThread} argument. 
#' @param nThread Number of threads to parallelize across.
#' @param verbose Print messages.
#' @inheritParams call_peaks
#' @inheritParams get_geo_supplementary_files
#' @inheritParams BiocParallel::MulticoreParam
#' 
#' @returns 
#' A named list of peak files in  \link[GenomicRanges]{GRanges} format.
#' If peaks could not be recovered for a sample,
#'  that element will be filled with \code{NA}.
#' 
#' @export
#' @importFrom stats setNames
#' @importFrom GenomicRanges GRanges
#' @examples 
#' grl <- echoannot::import_peaks(ids = "GSM4271282")
import_peaks <- function(ids,
                         builds = "hg19",
                         query_granges = NULL, 
                         query_granges_build = NULL,
                         query_by_chromosome = FALSE,
                         force_new = FALSE, 
                         cutoff = NULL,
                         split_chromosomes = FALSE,
                         regex_queries = list(
                             narrowPeak="narrowpeak",
                             broadPeak="broadpeak",
                             genericPeak="peak",
                             bedGraph="bedgraph|graph.gz|bdg.gz",
                             bigWig="bigwig|bw$"
                         ),
                         peaks_dir = tempdir(),
                         save_path = tempfile(fileext = "grl.hg38.rds"),
                         nThread = 1,
                         verbose = TRUE){
    
    #### Check builds ####
    if(length(builds)>1 && (length(builds)!=length(ids))){
        stop("builds must be same length as ids.")
    } 
    #### Get GSM names ####
    ids <- process_ids(ids = ids, 
                       verbose = verbose) 
    #### Check query_granges ####
    if(!is.null(query_granges)){
        ## Standardise to UCSC style 
        query_granges <- echodata::dt_to_granges(query_granges, 
                                                 style = "UCSC", 
                                                 verbose = FALSE)
        if(is.null(query_granges_build)){
            stop("query_granges_build must be set when using query_granges.")
        }
    } 
    #### Check for pre-existing data ####
    if((!is.null(save_path)) && 
       file.exists(save_path) & 
       isFALSE(force_new)){
        messager("Importing stored peaks data.",v=verbose)
        grl <- readRDS(save_path)
    } else { 
        ids <- stats::setNames(ids,ids) 
        messager("Querying",length(ids),"sample(s).",v=verbose)
        grl <- mapply(ids, FUN = function(no){
            cat(paste("\nQuery:",no,"\n"))
            tryCatch({
                #### Get genome build #### 
                build <- if(length(builds)==1) {
                    builds
                } else if (no %in% names(builds)){
                    builds[[no]]
                }
                messager("Using build:",build,v=verbose)
                #### Import or call peaks: GEO ####
                if(startsWith(toupper(no),"GSM")){ 
                    import_peaks_geo(gsm = no, 
                                     build = build,
                                     query_granges = query_granges,
                                     query_granges_build = query_granges_build,
                                     cutoff = cutoff,
                                     regex_queries = regex_queries,
                                     peaks_dir = peaks_dir,
                                     split_chromosomes = split_chromosomes,
                                     nThread = nThread,
                                     verbose = verbose) 
                #### Import peaks: ENCODE ####
                } else if(startsWith(toupper(no),"ENCF")){
                    messager("Under construction.")
                    return(GenomicRanges::GRanges())
                } else {
                    messager("id not recognized. Skipping:",no,
                             v=verbose)
                    return(GenomicRanges::GRanges())
                } 
            }, error=function(e) {message(e); GenomicRanges::GRanges()}) 
        })
        #### Save ####
        if(!is.null(save_path)){
            messager("Saving results ==> ",save_path,v=verbose)
            dir.create(dirname(save_path),
                       showWarnings = FALSE, recursive = TRUE)
            saveRDS(grl, save_path)
        } 
    }
    return(grl)    
}
