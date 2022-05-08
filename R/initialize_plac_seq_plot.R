initialize_plac_seq_plot <- function(interact.DT,
                                     highlight_plac,
                                     genomic_units,
                                     max.height,
                                     interact_y,
                                     strip.text.y.angle,
                                     verbose = TRUE){
    requireNamespace("ggplot2")
    messager("Initializing PLAC-seq plot.",v=verbose) 
    if (highlight_plac) {
        ##### Method 1 #####
        ### Apply alpha based on consensus SNP overlap
        NOTT.interact_trk <-
            ggbio::ggbio() +
            ggbio::geom_arch(
                data = interact.DT,
                ggplot2::aes_string(
                    x = "Start",
                    y = interact_y,
                    xend = "End",
                    alpha = "consensus_snp_overlap"
                ),
                max.height = max.height, colour = "black"
            ) + 
            ggplot2::scale_alpha_manual("Consensus SNP overlaps", 
                                        values = c(0.05, 1)) +
            ggplot2::labs(y = NULL)
    } else {
        ##### Method 2 #####
        ### Apply alpha to all interactions
        NOTT.interact_trk <-
            ggbio::ggbio() +
            ggbio::geom_arch(
                data = interact.DT, alpha = 0.25, color = "black",
                max.height = max.height,
                ggplot2::aes_string(x = "Start",
                                    y = interact_y,
                                    xend = "End")
            ) +
            ggplot2::labs(y = NULL)
    }
    #### Post-processing ####
    NOTT.interact_trk <- suppressMessages(
        NOTT.interact_trk +
            ggplot2::facet_grid(facets = Cell_type ~ .) +
            ggplot2::scale_y_reverse() +
            ggplot2::theme_classic() +
            ggplot2::theme(
                legend.key.width = ggplot2::unit(1.5, "line"),
                legend.key.height = ggplot2::unit(1.5, "line"),
                axis.text.y = ggplot2::element_blank(),
                strip.text.y = ggplot2::element_text(
                    angle = strip.text.y.angle)
            )
    )
    if(genomic_units=="Mb"){
        NOTT.interact_trk <- ggbio_to_mb(gg = NOTT.interact_trk,
                                         verbose = verbose) 
    } 
    return(NOTT.interact_trk)
}