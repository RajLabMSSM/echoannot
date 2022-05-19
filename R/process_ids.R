process_ids <- function(ids,
                        verbose=TRUE){
    # ids=c("GSE188512","GSM5684359")
    messager("Processing ids.",v=verbose)
    new_ids <- lapply(ids, function(id){
        #### Get GSM samples from GSE id ####
        if(grepl("^GSE", id, ignore.case = TRUE)){
            g <- GEOquery::getGEO(GEO = "GSE188512")
            samples <- unlist(
                lapply(g, function(x){
                    x[["geo_accession"]]
                }) 
            ) 
            names(samples) <- rep("GEO",length(samples))
        } else if(grepl("^GSM", id, ignore.case = TRUE)){
            samples <- c("GEO"=id)
        } else {
            messager("id type unreconized, omitting:",id,v=verbose)
            samples <- NULL
        }
        return(samples)
    })
    #### Get unique ids while keeping names ####
    new_ids <- unlist(new_ids)[!duplicated(unlist(new_ids))] 
    messager(length(new_ids),"unique ids identified.")
    return(new_ids)
}
