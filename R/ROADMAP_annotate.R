ROADMAP_annotate <- function(grlist,
                             ref=NULL,
                             keyword_query=NULL,
                             sep="\n",
                             verbose=TRUE){
    
    EID <- NULL;
    
    messager("ROADMAP:: Annotating GRangesList.",v=verbose)
    if(is.null(ref)){
        ref <- ROADMAP_construct_reference(keyword_query = keyword_query, 
                                           verbose = verbose)
    } 
    grl2 <- lapply(grlist, 
                   function(gr){
         eid <- gr$EID[1]
        ref_sub <- ref[EID==eid,]
        GenomicRanges::mcols(gr)$Source <- 
            gsub("[ ]|_",sep,
                 paste(eid,
                       ref_sub$`Standardized Epigenome name (from Wouter)`,
                       sep = sep))
        return(gr)
    })
    return(grl2) 
}