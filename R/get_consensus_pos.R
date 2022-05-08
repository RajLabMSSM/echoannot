get_consensus_pos <- function(dat, 
                              xvar){
    Consensus_SNP <- NULL;
    
    consensus.pos <- subset(dat, Consensus_SNP == TRUE)[[xvar]] 
    return(consensus.pos)
}