get_consensus_pos <- function(dat, 
                              xvar){
    consensus.pos <- subset(dat, Consensus_SNP == TRUE)[[xvar]] 
    return(consensus.pos)
}