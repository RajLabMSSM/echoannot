#' Get top consensus position
#' 
#' Get the genomic position of the top fine-mapped Consensus SNP.
#' @param dat Data
#' @param xvar Genomic position column name.
#' @keywords internal
#' @importFrom dplyr top_n
get_top_consensus_pos <- function(dat,
                                  xvar){ 
    mean.PP <- Effect <- Consensus_SNP <- Support <- NULL;
    
    consensus.pos <- get_consensus_pos(dat = dat, 
                                       xvar = xvar)
    if (length(consensus.pos) > 0) {
        top.consensus.pos <-
            (dplyr::top_n(subset(dat, Consensus_SNP == TRUE),
                          n = 1, wt = mean.PP
            ) |>
                dplyr::top_n(1, wt = Effect))[[xvar]][1]
    } else {
        top.consensus.pos <-
            (dplyr::top_n(subset(dat, Support > 0),
                          n = 1, wt = mean.PP
            ) |>
                dplyr::top_n(1, wt = Effect))[[xvar]][1]
    }
    return(top.consensus.pos)
}
