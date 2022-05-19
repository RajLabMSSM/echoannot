import_peaks_bedgraph <- function(paths, 
                                  id,
                                  query_granges, 
                                  build,
                                  cutoff,
                                  peaks_dir,
                                  verbose=TRUE){
    #### Import bedGraph subset #### 
    peaks_all <- lapply(paths, function(x){
        if(!is.null(query_granges)){
            ## Import the entire chromosome to accurately compute peaks.
            chroms <- levels(GenomicRanges::seqnames(query_granges))
            gr <- import_bedgraph_chroms(URL = x, 
                                         chroms = chroms, 
                                         build = build, 
                                         import_format = "bedGraph", 
                                         verbose = verbose) 
        } else {
            ## Import the entire genome.
            chroms <- "chrALL"
            gr <- rtracklayer::import.bedGraph(con = x)
        } 
        #### Save lifted subset ####
        messager("Writing (lifted) bedGraph subset.",v=verbose)
        tmp_lifted <- tempfile(
            fileext = paste(id,"lifted.bedgraph",sep=".")
        )
        rtracklayer::export.bedGraph(object = gr,
                                     con = tmp_lifted)
        #### Call peaks ####
        peaks <- call_peaks(bedgraph_path = tmp_lifted,
                            cutoff = cutoff,
                            outdir = peaks_dir,
                            outputfile = paste(
                                id,
                                paste(chroms,collapse = ";"),
                                "peaks.bed",
                                sep=".")
        ) 
        return(peaks)
    }) %>% 
        GenomicRanges::GRangesList() %>%
        unlist()
    ### Add to list ###
    GenomicRanges::mcols(peaks_all)$peaktype <- "MACSrPeak_bedgraph"
    return(peaks_all)
}
