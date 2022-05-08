#' Import peaks: GEO
#' 
#' Import narrow/broad/generic peaks from GEO, or compute peaks with 
#' \link[echoannot]{call_peaks}.
#' 
#' Must import \link[methods]{new} in my function
#' because it seems \link[GEOquery]{getGEO} forgot to do this 
#' (only works when you load the entire \pkg{GEOquery} package first).
#' 
#' @param gsm GEO GSM id (e.g. "GSM4271282").
#' @inheritParams import_peaks
#' 
#' @returns Named list of peak files in \link[GenomicRanges]{GRanges} format.
#' 
#' @keywords internal 
#' @import BiocGenerics 
#' @importFrom GenomicRanges seqnames mcols GRangesList
#' @importFrom rtracklayer import import.bedGraph
#' @importFrom data.table fread 
#' @importFrom echotabix liftover
#' @importFrom echodata dt_to_granges
import_peaks_geo <- function(gsm,  
                             build,
                             query_granges,
                             query_granges_build,
                             cutoff = NULL,
                             peaks_dir = tempdir(),
                             regex_queries = list(
                                 narrowPeaks="narrowpeak",
                                 broadPeaks="broadpeak",
                                 genericPeaks="peak",
                                 bedGraph="bedgraph|graph.gz|bdg.gz",
                                 bigWig="bigwig|bw$"
                             ),
                             verbose = TRUE){
    
    messager("Determining available file types.",v=verbose) 
    #### Determine which chroms to query ####
    chroms <- if(!is.null(query_granges)){
        unique(GenomicRanges::seqnames(query_granges))    
    } else{NULL} 
    #### Get links to supplementary files on GEO ####
    links <- get_geo_supplementary_files(gsm = gsm,
                                         regex_queries = regex_queries,
                                         verbose = verbose) 
    #### If files include narrowPeaks, import directly #### 
    if(length(links$narrowPeaks)>0){
        messager("Using pre-computed narrowPeak files.",v=verbose)
        peaks <- lapply(links$narrowPeaks, function(f){
            # p <- get_chroms(URL = f, chroms = chroms) 
            p <- rtracklayer::import(con = f, which = query_granges)
            GenomicRanges::mcols(p)$source <- basename(f)
            return(p)
        }) %>% 
            unlist() %>% 
            GenomicRanges::GRangesList() %>% 
            unlist()
        #### Liftover (if necessary) #### 
        if(!is.null(query_granges_build)){
            peaks <- echotabix::liftover(dat =  peaks, 
                                         query_genome = build,
                                         target_genome = query_granges_build, 
                                         as_granges = TRUE, 
                                         style = "UCSC")
        } 
        
    #### If files include broadPeaks, import directly #### 
    } else if(length(links$broadPeaks)>0){
        messager("Using pre-computed broadPeak files.",v=verbose)
        peaks <- lapply(links$broadPeaks, function(f){
            # p <- get_chroms(URL = f,  chroms = chroms) 
            p <- rtracklayer::import(con = f, which = query_granges)
            GenomicRanges::mcols(p)$source <- basename(f)
            return(p)
        }) %>% 
            unlist() %>% 
            GenomicRanges::GRangesList() %>% 
            unlist()
        #### Liftover (if necessary) #### 
        if(!is.null(query_granges_build)){
            peaks <- echotabix::liftover(dat = peaks, 
                                         query_genome = build,
                                         target_genome = query_granges_build, 
                                         as_granges = TRUE, 
                                         style = "UCSC")
        } 
        
    #### If files include generic peaks, import directly #### 
    } else if(length(links$genericPeaks)>0){
        messager("Using pre-computed generic peak files.",v=verbose)
        peaks <- lapply(links$genericPeaks, function(f){
            tryCatch({
                ### not all files parse very cleanly 
                dat <- data.table::fread(f)
                col1 <- strsplit(colnames(dat)[1]," ")[[1]]
                if((length(col1)>1) && 
                   all(startsWith(colnames(dat)[-1],"V"))){
                    colnames(dat) <- tolower(gsub("^peak_","",col1[col1!=""],
                                                  ignore.case = TRUE))
                    colnames(dat) <- gsub("^chrom","chr",colnames(dat))
                }
                if(all(startsWith(colnames(dat),"V"))){
                    colnames(dat)[seq_len(3)] <- c("chr","start","end")
                } 
                p <- echodata::dt_to_granges(dat = dat, 
                                             chrom_col = "chr", 
                                             start_col = "start",
                                             end_col = "end", 
                                             style = "UCSC", 
                                             verbose = verbose) 
                GenomicRanges::mcols(p)$source <- basename(f)
                return(p)
            }, error = function(e){message(e);NULL})
        }) %>% 
            unlist() %>% 
            GenomicRanges::GRangesList() %>% 
            unlist() 
        #### Liftover (if necessary) #### 
        if(!is.null(query_granges_build)){
            peaks <- echotabix::liftover(dat = peaks, 
                                         query_genome = build,
                                         target_genome = query_granges_build, 
                                         as_granges = TRUE, 
                                         style = "UCSC")
        } 
    #### Call peaks from bedGraph #### 
    } else if(length(links$bedGraph)>0){
        messager("Computing peaks from bedGraph file.",v=verbose)
        #### Import bedGraph subset #### 
        if(!is.null(query_granges)){
            ## Import the entire chromosome to accurately compute peaks.
            gr <- import_bedgraph_chroms(URL = links$bedGraph[1], 
                                         chroms = chroms, 
                                         build = build, 
                                         import_format = "bedGraph", 
                                         verbose = verbose) 
        } else {
            ## Import the entire genome.
            gr <- rtracklayer::import.bedGraph(con = links$bedGraph[1])
        }
        #### Liftover (if necessary) #### 
        if(!is.null(query_granges_build)){
            gr <- echotabix::liftover(dat = gr, 
                                      query_genome = build,
                                      target_genome = query_granges_build, 
                                      as_granges = TRUE, 
                                      style = "UCSC")
        } 
        #### Save lifted subset ####
        messager("Writing (lifted) bedGraph subset.",v=verbose)
        tmp_lifted <- tempfile(
            fileext = paste(gsm,"lifted.bedgraph",sep=".")
        )
        rtracklayer::export.bedGraph(object = gr,
                                     con = tmp_lifted)
        #### Call peaks ####
        peaks <- call_peaks(bedgraph_path = tmp_lifted,
                            cutoff = cutoff,
                            outdir = peaks_dir,
                            outputfile = paste(
                                gsm,
                                paste(chroms,collapse = ";"),
                                "peaks.bed",
                                sep=".")
        ) 
    #### Call peaks from bigWig #### 
    } else if(length(links$bigWig)>0){
        messager("Computing peaks from bigWig file.",v=verbose)
        #### Import bigWig subset #### 
        if(!is.null(query_granges)){
            ## Import the entire chromosome to accurately compute peaks.
            gr <- import_bedgraph_chroms(URL = links$bigWig[1], 
                                         chroms = chroms, 
                                         build = build, 
                                         import_format = "BigWig",
                                         verbose = verbose) 
        } else {
            ## Import the entire genome.
            gr <- rtracklayer::import.bw(con = links$bigWig[1])
        }
        #### Fix seqinfo ####
        gr <- fix_seqinfo(gr = gr, 
                          build = build, 
                          verbose = verbose)
        #### Liftover (if necessary) #### 
        if(!is.null(query_granges_build)){
            gr <- echotabix::liftover(dat = gr, 
                                      query_genome = build,
                                      target_genome = query_granges_build, 
                                      as_granges = TRUE, 
                                      style = "UCSC")
        }  
        #### Save lifted subset ####
        messager("Writing (lifted) bigWig subset as bedGraph.",v=verbose)
        tmp_lifted <- tempfile(fileext = paste(gsm,"lifted.bedgraph",sep="."))
        rtracklayer::export.bedGraph(object = gr,
                                     con = tmp_lifted)
        #### Call peaks ####
        peaks <- call_peaks(bedgraph_path = tmp_lifted,
                            cutoff = cutoff,
                            outdir = peaks_dir,
                            outputfile = paste(
                                gsm,
                                paste(chroms,collapse = ";"),
                                "peaks.bed",
                                sep=".")
        ) 
    } else{ 
        messager("Could not find any files to import peaks from.",
                 "Returning NA.",v=verbose)
        return(NA)
    }
    #### Subset peaks ####
    if(!is.null(query_granges)){
        peaks <- subset_peaks(peaks = peaks,
                              query_granges = query_granges)
    } 
    return(peaks) 
}
