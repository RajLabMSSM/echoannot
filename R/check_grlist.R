check_grlist <- function(grlist){
    grlist <- if(methods::is(grlist,"GRanges")){
        list(item1=grlist)
    } else if(methods::is(grlist,"GRangesList")){
        as.list(grlist)
    } else if(methods::is(grlist,"list")){
        if(is.null(names(grlist))){
            names(grlist) <- paste0("item",seq_len(length(grlist)))
        }
        grlist
    } else {
        stp <- paste("grlist must be one of the following:",
                     paste("\n -",c("GRanges","GRangesList","list"), collapse = "")
                     )
        stop(stp)
    }
    #### Standardise style ####
    grlist <- mapply(grlist, FUN=function(x){
        echodata::dt_to_granges(dat = x,
                                style = "UCSC",
                                verbose = FALSE) 
    })
    return(grlist)
}
