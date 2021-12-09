ggbio_to_mb <- function(gg,
                        verbose = TRUE){ 
        messager("++ Converting ggbio genomic units to Mb.", v = verbose)
        gg <- gg +
            ggbio::scale_x_sequnit(unit = "Mb") 
        return(gg)
}