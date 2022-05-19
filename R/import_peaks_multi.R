import_peaks_multi <- function(links,
                               id=basename(tempfile()),
                               build = "hg19",
                               query_granges = NULL,
                               query_granges_build = NULL,
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
                               merge_list = TRUE,
                               nThread = 1,
                               verbose = TRUE){
    ## BiocParallel is a bad idea here,
    ## since there's some functions within the loop that are already 
    ## parallelized, causing conflicts?
    # BiocParallel::register(
    #     BiocParallel::MulticoreParam(workers = workers,
    #                                  progressbar = verbose)    
    # )
    
    #### Liftover (if necessary) ####
    if(!is.null(query_granges)){
        query_granges <- echotabix::liftover(dat = query_granges, 
                                             query_genome = query_granges_build,
                                             target_genome = build, 
                                             as_granges = TRUE, 
                                             style = "UCSC")
        query_granges_build <- build
    }
    #### Split up chromosomes ####
    if(split_chromosomes){ 
        if(is.null(query_granges)){
            query_granges <- get_genome(build = build)
        }
        query_granges_list <- split_chromosomes_run(query_granges = query_granges)
    } else {
        query_granges_list <- list(all=query_granges)
    }
    #### Iterate over chromosomes #### 
    peaks_all <- parallel::mclapply(X = query_granges_list,
                                    mc.cores = nThread,
                                    FUN = function(query_granges){
                                        
        #### Intialize GRanges object ####                                             
        peaks_l <- GenomicRanges::GRanges()
        #### Determine which chrom is being queried #### 
        if(nThread>1 && split_chromosomes) {
            chrom <- unique(GenomicRanges::seqnames(query_granges))
            message_parallel("Processing: ",paste(chrom,collapse = ",")) 
        }
        ## Use this bc right now "narrowPeaks.tsv" would be imported by both
        ## narrowpeak/genericpeak functions.  
        processed <- FALSE;
        
        #### If files include narrowPeak, import directly #### 
        if(length(links$narrowpeak)>0){
            peaks <- import_peaks_narrowpeak(paths = links$narrowpeak,
                                             query_granges = query_granges,
                                             verbose = verbose)
            processed <- TRUE
            peaks_l <- c(peaks_l, peaks)
        } 
        #### If files include broadPeak, import directly #### 
        if(length(links$broadpeak)>0){
            peaks <- import_peaks_broadpeak(paths = links$broadpeak,
                                            query_granges = query_granges,
                                            verbose = verbose)
            processed <- TRUE
            peaks_l <- c(peaks_l, peaks)
        }  
        #### If files include generic peaks, import directly #### 
        if(length(links$genericpeak)>0 && isFALSE(processed)){ 
            nThread_dt <- if(split_chromosomes) 1 else nThread
            peaks <- import_peaks_genericpeak(paths = links$genericpeak,
                                              nThread = nThread_dt,
                                              verbose = verbose)
            peaks_l <- c(peaks_l, peaks) 
        }
        
        #### Call peaks from bedGraph #### 
        if(length(links$bedgraph)>0){
            messager("Computing peaks from bedGraph file.",v=verbose)
            peaks <- import_peaks_bedgraph(paths=links$bedgraph, 
                                           id=id, 
                                           query_granges=query_granges,
                                           build=build,
                                           cutoff=cutoff,
                                           peaks_dir=peaks_dir,
                                           verbose=verbose)
            peaks_l <- c(peaks_l, peaks)
        }
        
        #### Call peaks from bigWig #### 
        if(length(links$bigwig)>0){
            peaks <- import_peaks_bigwig(paths=links$bigwig,
                                         id=id,
                                         query_granges=query_granges,
                                         build=build,
                                         cutoff=cutoff,
                                         peaks_dir=peaks_dir,
                                         verbose=verbose)
            peaks_l <- c(peaks_l, peaks)  
        } 
        #### Post-processing ####
        if(length(peaks_l)==0) {
            message_parallel("No peaks identified.")
            # Skip filtering if empty
            return(peaks_l)
        }
        #### Subset peaks ####
        if(!is.null(query_granges)){
            peaks_l <- subset_peaks(peaks = peaks_l,
                                    query_granges = query_granges)
        } 
        return(peaks_l)
    })  # <-- End parallel loop
    
    #### Merge list ####
    if(merge_list){
        peaks_all <- unlist(GenomicRanges::GRangesList(peaks_all)) 
    } 
    return(peaks_all)
}
