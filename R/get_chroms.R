get_chroms <- function(URL, 
                       chroms = NULL,
                       build = "hg38",
                       import_format = "bedGraph",
                       export_format = import_format,
                       save_path = NULL,
                       verbose = TRUE){
    
    # save_path <- file.path(
    #     tempdir(), 
    #     paste(
    #         gsub(".bedgraph.gz$|.graph.gz$","",basename(URL)),
    #         "selected_chroms","bedgraph",sep = ".")
    # )
    # chroms <- "chr6" 
    messager("Importing chromosomes:",paste(chroms,collapse = ","),
             v=verbose)
    chroms <- unique(as.character(chroms))
    all_chrom <- regioneR::getGenome(genome = build)
    messager("+ Selecting available chromosomes.",v=verbose)
    ## WARNING! BiocGenerics::%in% does not work properly 
    ## and interferes with S4Vectors::%in% when loaded.
    ## Use grepl instead.
    select_chrom <- if(!is.null(chroms)){
        all_chrom[grepl(paste(paste0("^",chroms,"$"),collapse = "|"),
                        as.character(GenomicRanges::seqnames(all_chrom))), ]
    } else {all_chrom} 
    if(length(select_chrom)==0) {
        stop("No matching chromosomes could be identified.")
    }
    messager("+ Importing as:",import_format,v=verbose)
    gr <- rtracklayer::import(con = URL, 
                              which = select_chrom,
                              format = import_format)
   #### Save or return directly ####
    if(!is.null(save_path)) { 
        messager("+ Writing chromosome subset ==>",save_path, v=verbose)
        rtracklayer::export(object = gr, 
                            con = save_path, 
                            format = export_format)
        return(save_path)
    } else {
        messager("Returning,",import_format,"of length",length(gr),v=verbose)
        return(gr)
    } 
}
