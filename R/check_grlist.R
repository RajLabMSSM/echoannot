check_grlist <- function(grlist,
                         style="UCSC",
                         prefix=NULL,
                         verbose=TRUE){
    
    
    grlist <- if(methods::is(grlist,"data.frame")){
        list(item1=echodata::dt_to_granges(dat = grlist,
                                         style = style,
                                         verbose = FALSE))
    } else if(methods::is(grlist,"GRanges")){
        list(item1=grlist)
    } else if(methods::is(grlist,"GRangesList")){
        as.list(grlist)
    } else if(methods::is(grlist,"list")){
        if(is.null(names(grlist))){
            names(grlist) <- paste0(prefix,"_",seq_len(length(grlist)))
        }
        grlist <- lapply(grlist, function(gr){
            echodata::dt_to_granges(dat = gr,
                                    style = style,
                                    verbose = FALSE) 
        })
        return(grlist)
    } else {
        stp <- paste("grlist must be one of the following:",
                     paste("\n -",c("GRanges","GRangesList","list"), collapse = "")
                     )
        stop(stp)
    }
    #### Standardise style ####
    grlist <- mapply(grlist, FUN=function(x){
        echodata::dt_to_granges(dat = x,
                                style = style,
                                verbose = FALSE) 
    })
    return(grlist)
}
