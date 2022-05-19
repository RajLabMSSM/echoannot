import_peaks_broadpeak <- function(paths,
                                   query_granges,
                                   verbose=TRUE){
    messager("Using pre-computed broadPeak files.",v=verbose)
    peaks <- lapply(paths, function(f){ 
        p <- rtracklayer::import(con = f, 
                                 which = query_granges,
                                 format = "broadPeak")
        GenomicRanges::mcols(p)$source <- basename(f)
        return(p)
    }) %>% 
        unlist() %>% 
        GenomicRanges::GRangesList() %>% 
        unlist() 
    ### Add to list ###
    GenomicRanges::mcols(peaks)$peaktype <- "broadPeak"
    return(peaks)
}