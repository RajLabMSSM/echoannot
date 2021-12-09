as_ggbio <- function(plot_list, 
                     x_limits = NULL,
                     verbose = TRUE){
    messager("Converting plots to a named list of ggbio objects.",
             v=verbose)
    if(!is.list(plot_list)) plot_list <- list("plot"=plot_list) 
    ggbio_list <- lapply(plot_list, function(x){
        y <- if(!methods::is(x,"GGbio")) {
            ggbio::ggbio(x)
        } else {x}
        if(!is.null(x_limits)) y <- y + ggplot2::xlim(x_limits)
        return(y)
    })
    names(ggbio_list) <- names(plot_list)
    return(ggbio_list)
}