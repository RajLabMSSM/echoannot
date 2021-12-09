as_patchwork <- function(plot_list,
                         ncol = 1,
                         tag_levels = NULL,
                         x_limits = NULL, 
                         verbose = TRUE,
                         ...){
    messager("Converting plots to a merged patchwork object.",
             v=verbose) 
    #### Recursively convert each to ggbio/ggplot format first ####
    plot_list <- convert_plots(plot_list = plot_list,
                               return_as = "ggplot",
                               x_limits = x_limits,
                               verbose = FALSE)
    patch <- patchwork::wrap_plots(plot_list,
                                   ncol = ncol,
                                   ...) 
    if(!is.null(tag_levels)){
        patch <- patch + patchwork::plot_annotation(
            tag_levels = tag_levels
        )
    }
    # if(!is.null(x_limits)){
    #     patch <- patch & ggplot2::xlim(x_limits)
    # }
    return(patch)
}