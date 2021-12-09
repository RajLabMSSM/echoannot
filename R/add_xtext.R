add_xtext <- function(NOTT.interact_trk,
                      verbose = TRUE){
    requireNamespace("ggplot2")
    messager("++ Removing xtext.",v=verbose)
    NOTT.interact_trk <- NOTT.interact_trk +
        ggplot2::theme(
            axis.text.x = ggplot2::element_blank(),
            axis.title.x = ggplot2::element_blank()
        )
    return(NOTT.interact_trk)
}