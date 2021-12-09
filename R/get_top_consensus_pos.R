get_top_consensus_pos <- function(dat,
                                  xvar){ 
    consensus.pos <- get_consensus_pos(dat = dat, 
                                       xvar = xvar)
    if (length(consensus.pos) > 0) {
        top.consensus.pos <-
            (dplyr::top_n(subset(dat, Consensus_SNP == TRUE),
                          n = 1, wt = mean.PP
            ) %>%
                dplyr::top_n(1, wt = Effect))[[xvar]][1]
    } else {
        top.consensus.pos <-
            (dplyr::top_n(subset(dat, Support > 0),
                          n = 1, wt = mean.PP
            ) %>%
                dplyr::top_n(1, wt = Effect))[[xvar]][1]
    }
    return(top.consensus.pos)
}