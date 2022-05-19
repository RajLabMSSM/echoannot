#' Call peaks
#' 
#' Call peaks from a bedGraph or bigWig file using \link[MACSr]{bdgpeakcall}, 
#' which is a wrapper for the \emph{MACS3} command line tool.
#' Can automatically infer a reasonable \code{cutoff} threshold as well.
#' 
#' @param bedgraph_path Path to bedGraph file. 
#' Can instead provide a bigWig file, 
#' but this will first be converted to bedGraph format, 
#' which can take some time if trying to convert data from across the entire
#' genome.
#' @param cutoff Cutoff depends on which method you used for score track.
#'  If the file contains pvalue scores from MACS3, score 5 means pvalue 1e-5.
#'  If \code{NULL}, a reasonable \code{cutoff} value will be inferred 
#'  through a \code{cutoff_analysis}. 
#' @param outdir Directory to store \code{cutoff_analysis} 
#' report and peak file in.
#' @param outputfile Name of the peak output file (stored in BED format).
#' @param return_path Whether to return the path to the saved peak file, 
#' or the peak data itself as a \link[GenomicRanges]{GRanges} object.
#' @param verbose Print messages.
#' @inheritParams MACSr::bdgpeakcall
#' 
#' @export
#' @importFrom rtracklayer import.bed import export.bedGraph
#' @importFrom echodata is_local
#' @importFrom R.utils isGzipped gunzip 
#' @importFrom utils download.file
#' @importFrom data.table fread
#' @importFrom stats median
#' @examples
#' #### Get bedGraph subset ####
#' ## Normally, you'd call peaks on the entire chromosome, 
#' ## or even the whole genome. But for demo purposes we'll just use one locus.
#' gsm <- "GSM4703766" 
#' links <- echoannot:::get_geo_supplementary_files(gsm = gsm) 
#' query_granges <- GenomicRanges::GRanges("chr6:165169213-167169213")
#' gr <- rtracklayer::import(con = links$bedGraph, which = query_granges)
#' tmp <- tempfile(fileext = ".bedgraph")
#' rtracklayer::export.bedGraph(object = gr, con = tmp)
#' 
#' #### Call peaks #### 
#' peaks <- echoannot::call_peaks(bedgraph_path = tmp)
call_peaks <- function(bedgraph_path,
                       cutoff = NULL,
                       minlen = 200L,
                       maxgap = 30L,
                       call_summits = TRUE,
                       trackline = TRUE,
                       log = TRUE,
                       outdir = tempdir(),
                       outputfile = "MACSr.peaks.bed", 
                       return_path = FALSE,
                       verbose = TRUE){
    
    requireNamespace("MACSr")
    
    #### Download ####
    if(!echodata::is_local(bedgraph_path)){
        bedgraph_path2 <- file.path(tempdir(),basename(bedgraph_path))
        utils::download.file(url = bedgraph_path, 
                             destfile = bedgraph_path2)
        if(R.utils::isGzipped(bedgraph_path2)){
            bedgraph_path2 <- R.utils::gunzip(bedgraph_path2, overwrite=TRUE)
        } 
        bedgraph_path <- bedgraph_path2
    }
    #### Convert to bedGRaph ####
    if(is_bigwig(bedgraph_path)){ 
        messager("Converting bigWig --> bedGraph.",v=verbose)
        gr <- rtracklayer::import(bedgraph_path)
        bedgraph_path2 <- gsub("bigwig$|bw$","bedGraph",bedgraph_path,
                               ignore.case = TRUE)
        rtracklayer::export.bedGraph(object = gr, con = bedgraph_path2)
        bedgraph_path <- bedgraph_path2
    }
    #### Determine cutoff threshold autmatically ####
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
    if(is.null(cutoff)){
        messager("Analyzing cutoff thresholds.",v=verbose)
        out_cutoff <- MACSr::bdgpeakcall(ifile = bedgraph_path, 
                                         outdir = outdir,
                                         cutoff_analysis = TRUE, 
                                         outputfile = gsub(
                                             ".bed$",".cutoff_analysis.txt",
                                             outputfile)
                                         )
        cutoffs <- data.table::fread(out_cutoff$outputs)
        cutoff <- stats::median(cutoffs$pscore)
        # qplot(as.factor(cutoffs$pscore), cutoffs$avelpeak, geom="violin")
    } 
    #### Call peaks #####
    messager("Calling peaks.",v=verbose)
    out <- MACSr::bdgpeakcall(ifile = bedgraph_path, 
                              cutoff = cutoff, 
                              call_summits = call_summits,
                              trackline = trackline,
                              log = log,
                              outdir = outdir,
                              outputfile = outputfile)
    if(return_path) {
        return(out$outputs)
    } else {
        peaks <- rtracklayer::import.bed(out$outputs)
        messager(formatC(length(peaks),big.mark = ","),"peaks called.",
                 v=verbose)
        return(peaks)
    }
}
