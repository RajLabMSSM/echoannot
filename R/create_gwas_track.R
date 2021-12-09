create_gwas_track <- function(dat,
                              genomic_units,
                              point_size,
                              verbose = TRUE){
    messager("Creating GWAS track.",v=verbose)
    gg <- ggplot2::ggplot(
        data = dat,
        ggplot2::aes_string(x = "POS",
                            y = "-log10(P)", color = "-log10(P)")
    ) +
        ggplot2::geom_point(alpha = .5, shape = 16, size = point_size) +
        ggplot2::theme_classic()
    if(genomic_units=="Mb"){
        gg <- ggbio_to_mb(gg = gg, 
                          v=verbose)
    }
    return(gg)
}