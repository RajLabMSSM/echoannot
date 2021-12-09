create_finemap_track <- function(dat,
                                 genomic_units = "POS",
                                 point_size,
                                 verbose = TRUE){
    messager("Creating fine-map track.",v=verbose)
    gg <- ggplot2::ggplot(
        data = dat,
        ggplot2::aes_string(x = "POS", 
                            y = "mean.PP", color = "mean.PP")
    ) +
        ggplot2::geom_point(alpha = .5, shape = 16, size = point_size) +
        ggplot2::scale_color_viridis_c(
            breaks = c(0, 0.5, 1),
            limits = c(0, 1)
        ) +
        ggplot2::ylim(0, 1) + 
        ggplot2::theme_classic()
    
    if(genomic_units=="Mb"){
        gg <- ggbio_to_mb(gg = gg, 
                          v=verbose)
    }
    return(gg)
}