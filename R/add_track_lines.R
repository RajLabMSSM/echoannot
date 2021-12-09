add_track_lines <- function(trks,
                            lead.pos,
                            consensus.pos,
                            verbose = TRUE){
    messager("Adding vertical track lines for LeadSNP and Consensus_SNP",
             v=verbose)
    trks_plus_lines <- trks +
        ggplot2::geom_vline(
            xintercept = lead.pos,
            color = "red", 
            alpha = 1, size = 0.3,
            linetype = "solid"
        ) +
        ggplot2::geom_vline(
            xintercept = consensus.pos, 
            color = "goldenrod2",
            alpha = 1, size = 0.3, 
            linetype = "solid"
        ) 
    return(trks_plus_lines)
}