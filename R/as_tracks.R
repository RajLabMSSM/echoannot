as_tracks <- function(plot_list, 
                      x_limits = NULL,
                      params_list = NULL,
                      verbose = TRUE){ 
    messager("Converting plots to a merged ggbio Tracks object.",
             v=verbose)
    if(is.null(params_list)){
        params_list <- list(
            title = "",
            track.bg.color = "transparent",
            track.plot.color = "transparent",
            label.text.cex = 0.7,
            label.bg.fill = "grey12",
            label.text.color = "white",
            label.text.angle = 0,
            label.width = ggplot2::unit(5.5, "lines"),
            xlim = NULL
        )
    }
    if(!is.null(x_limits)) params_list$xlim <- x_limits
    if(is.null(params_list$xlim)){
        params_list$xlim <- limits_from_ggplot(gg = plot_list[[1]])
    }
    plot_list <- append(plot_list, params_list)
    tracks <- get("tracks", asNamespace("ggbio"))
    trks <- suppressMessages(do.call("tracks", plot_list)) 
    return(trks)
}
