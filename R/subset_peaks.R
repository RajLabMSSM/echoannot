subset_peaks <- function(peaks,
                         query_granges,
                         verbose = TRUE){ 
    
    messager("Subsetting peaks.",v=verbose)
    GenomicRanges::mcols(peaks)$peakwidth <- GenomicRanges::width(peaks)
    subsetByOverlaps <- get(
        "subsetByOverlaps",
        asNamespace("GenomicRanges")
    )
    hits <- subsetByOverlaps(peaks, query_granges)
    return(hits)
}
