import_peaks_narrowpeak <- function(paths,
                                    query_granges,
                                    verbose=TRUE){
    messager("Using pre-computed narrowPeak files.",v=verbose)
    peaks <- lapply(paths, function(f){ 
        p <- rtracklayer::import(con = f, 
                                 which = query_granges, 
                                 format = "narrowPeak")
        GenomicRanges::mcols(p)$source <- basename(f)
        return(p)
    }) %>% 
        unlist() %>% 
        GenomicRanges::GRangesList() %>% 
        unlist() 
    ### Add to list ###
    GenomicRanges::mcols(peaks)$peaktype <- "narrowPeak"
    return(peaks)
}
