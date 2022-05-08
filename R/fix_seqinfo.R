#' Fix bigWig
#' 
#' Fix issues with \link[GenomicRanges]{GRanges} object
#' imported by \code{rtracklayer::import}, 
#' Original solution described 
#' \href{https://www.biostars.org/p/374709/#374727 }{here}.
#' 
#' @param gr \link[GenomicRanges]{GRanges} object.
#' @param build Genome build.
#' @param verbose Print messages.
#' @keywords internal
fix_seqinfo <- function(gr, 
                        build = "hg19",
                        verbose = TRUE){
    
    messager("Fixing GRanges seqinfo.",v=verbose)
    build <- tolower(build)[1]
    if(build=="hg19"){
        requireNamespace("TxDb.Hsapiens.UCSC.hg19.knownGene")
        txdb <- TxDb.Hsapiens.UCSC.hg19.knownGene::TxDb.Hsapiens.UCSC.hg19.knownGene
    } else if (build=="hg38"){
        requireNamespace("TxDb.Hsapiens.UCSC.hg38.knownGene")
        txdb <- TxDb.Hsapiens.UCSC.hg38.knownGene::TxDb.Hsapiens.UCSC.hg38.knownGene
    } else {
        stop("build must be one of: 'hg18', 'hg38'")
    } 
    GenomicRanges::seqinfo(gr) <- GenomicRanges::seqinfo(txdb)[
        GenomicRanges::seqnames(GenomicRanges::seqinfo(gr))
    ] 
    return(gr)
}
