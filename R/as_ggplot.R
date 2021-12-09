as_ggplot <- function(plot_list, 
                      x_limits = NULL,
                      verbose = TRUE){
    messager("Converting plots to a named list of ggplot objects.",
             v=verbose)
    #### Ensure plot_list is a named list ####
    if(methods::is(plot_list,"Tracks")){
        plot_list <- tracks_to_ggplot_list(trks = plot_list)
    }
    if(!is.list(plot_list)) plot_list <- list("plot"=plot_list) 
    ggplot_list <- lapply(plot_list, function(x){
        y <- if(methods::is(x,"GGbio")) {
            x@ggplot
        } else {x}
        if(!is.null(x_limits)) y <- y + ggplot2::xlim(x_limits)
        return(y)
    }) 
    names(ggplot_list) <- names(plot_list)
    return(ggplot_list)
}