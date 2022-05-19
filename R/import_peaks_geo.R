#' Import peaks: GEO
#' 
#' Import narrow/broad/generic peaks from GEO, or compute peaks with 
#' \link[echoannot]{call_peaks}.
#' 
#' Must import \link[methods]{new} in my function
#' because it seems \link[GEOquery]{getGEO} forgot to do this 
#' (only works when you load the entire \pkg{GEOquery} package first).
#' @source \href{https://github.com/ropensci/rtweet/issues/229}{
#' curl connection timeout issues}
#' @source \href{https://github.com/Bioconductor/BiocParallel/issues/111}{
#' Issue with importing bigWig files in parallel}
#' @param gsm GEO GSM id (e.g. "GSM4271282").
#' @inheritParams import_peaks
#' @inheritParams get_geo_supplementary_files
#' @inheritParams base::options 
#' 
#' @returns Named list of peak files in \link[GenomicRanges]{GRanges} format.
#' 
#' @keywords internal 
#' @importFrom BiocGenerics %in%
#' @importFrom GenomicRanges seqnames mcols GRangesList GRanges mcols
#' @importFrom rtracklayer import import.bedGraph export.bedGraph
#' @importFrom data.table fread 
#' @importFrom echotabix liftover
#' @importFrom echodata dt_to_granges
#' @importFrom httr set_config config
#' @importFrom BiocParallel bplapply register MulticoreParam
import_peaks_geo <- function(gsm,  
                             build,
                             query_granges,
                             query_granges_build,
                             split_chromosomes = FALSE,
                             cutoff = NULL,
                             regex_queries = list(
                                 narrowPeak="narrowpeak",
                                 broadPeak="broadpeak",
                                 genericPeak="peak",
                                 bedGraph="bedgraph|graph.gz|bdg.gz",
                                 bigWig="bigwig|bw$"
                             ),
                             peaks_dir = tempdir(),
                             timeout = 3*60,
                             nThread = 1,
                             verbose = TRUE){
   
    messager("Determining available file types.",v=verbose) 
    #### Set timeout ####
    options(timeout = timeout)
    # opts <- httr::timeout(seconds = timeout) 
    httr::set_config(config = httr::config(connecttimeout = timeout),
                     override = TRUE)  
    #### Get links to supplementary files on GEO ####
    links <- get_geo_supplementary_files(gsm = gsm,
                                         regex_queries = regex_queries,
                                         verbose = verbose)
    #### Import peaks ####
    peaks_all <- import_peaks_multi(links = links, 
                                    id = gsm,
                                    build = build,
                                    query_granges = query_granges,
                                    query_granges_build = query_granges_build,
                                    split_chromosomes = split_chromosomes,
                                    cutoff = cutoff,
                                    regex_queries = regex_queries,
                                    peaks_dir = peaks_dir,
                                    nThread = nThread,
                                    verbose = verbose)
    return(peaks_all) 
}
