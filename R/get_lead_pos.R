get_lead_pos <- function(dat,
                         xvar,
                         index_SNP){
    if (is.null(index_SNP)) {
        lead.pos <- subset(dat, leadSNP==TRUE)[[xvar]]
    } else {
        lead.pos <- subset(dat, SNP == index_SNP)[[xvar]]
    }
    return(lead.pos)
}