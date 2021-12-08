#' Convert ggbio tracks plot to ggplot2 list
#' 
#' Convert \link[ggbio]{tracks} plot to a named list of 
#' \link[ggplot2]{ggplot} objects.
#' @param trks ggbio tracks plot.
#' @param verbose Print messages.
#' @keywords internal
tracks_to_ggplot_list <- function(trks,
                                  verbose = TRUE){
    messager("Converting ggbio tracks plot to ggplot2 list.",
             v=verbose)
    gg_list <- lapply(names(trks@plot),
                      function(x){trks@plot[[x]]@ggplot}) 
    names(gg_list) <- names(trks@plot)
    return(gg_list)
}