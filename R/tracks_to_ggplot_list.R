#' Convert ggbio tracks plot to ggplot2 list
#' 
#' Convert \link[ggbio]{tracks} plot to a named list of 
#' \link[ggplot2]{ggplot} objects.
#' @param trks ggbio tracks plot. 
#' @keywords internal
tracks_to_ggplot_list <- function(trks){
    gg_list <- lapply(names(trks@plot),
                      function(x){
                          trk <- trks@plot[[x]]
                          if(ggplot2::is.ggplot(trk)){
                              return(trk)
                          } else {
                              return(trks@plot[[x]]@ggplot)
                          }
                      }) 
    names(gg_list) <- names(trks@plot)
    return(gg_list)
}