#' Call peaks
#' 
#' Call peaks from a bedGraph file using \link[MACSr]{bdgpeakcall}, 
#' which is a wrapper for the \emph{MACS3} command line tool.
#' Can automatically infer a reasonable \code{cutoff} threshold as well.
#' 
#' @param bedgraph_path Path to bedGraph file.
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
#' 
#' @export
#' @importFrom rtracklayer import.bed
#' @examples 
#' 
call_peaks <- function(bedgraph_path,
                       cutoff = NULL,
                       outdir = tempdir(),
                       outputfile = "bdg_peakcall.bed",
                       return_path = FALSE,
                       verbose = TRUE){
    
    requireNamespace("MACSr")
    
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
        cutoff <- median(cutoffs$pscore)
        # qplot(as.factor(cutoffs$pscore), cutoffs$avelpeak, geom="violin")
    } 
    #### Call peaks #####
    messager("Calling peaks.",v=verbose)
    out <- MACSr::bdgpeakcall(ifile = bedgraph_path, 
                              cutoff = cutoff,
                              minlen = 10L,
                              # maxgap = 1L,
                              call_summits = TRUE, 
                              outdir = outdir,
                              outputfile = outputfile)
    if(return_path) {
        return(out$outputs)
    } else {
        peaks <- rtracklayer::import.bed(out$outputs)
        return(peaks)
    }
}
