get_zoom_xlims <- function(lead.pos,
                           zoom_window,
                           verbose = TRUE){
    messager("Computing x-axis limits for zoom_window.",v=verbose)
    xlims <- c(lead.pos - as.integer(zoom_window / 2),
               lead.pos + as.integer(zoom_window / 2))
    return(xlims)
}