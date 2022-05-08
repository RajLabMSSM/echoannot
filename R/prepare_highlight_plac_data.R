prepare_highlight_plac_data <- function(dat,
                                        interact.DT,
                                        verbose = TRUE){
    Consensus_SNP <- NULL;
    
    messager("Preparing data for highlighting PLAC-seq interactions.",
             v=verbose)
    consensus_snps <- subset(dat, Consensus_SNP == TRUE)
    consensus_snps$CHR <- paste0("chr", consensus_snps$CHR)
    ## make GRanges
    consensus.gr <- GenomicRanges::GRanges(
        seqnames = consensus_snps$CHR,
        ranges = IRanges::IRanges(
            start = consensus_snps$POS - 1,
            end = consensus_snps$POS
        )
    )
    #### Convert to UCSC format ####
    consensus.gr <- echodata::dt_to_granges(dat = consensus.gr,
                                            style = "UCSC",
                                            verbose = FALSE)
    plac_start.gr <- GenomicRanges::GRanges(
        seqnames = interact.DT$chr,
        ranges = IRanges::IRanges(
            start = interact.DT$Start - 5000,
            end = interact.DT$Start
        )
    )
    plac_start.gr <- echodata::dt_to_granges(dat = plac_start.gr,
                                             style = "UCSC",
                                             verbose = FALSE)
    plac_end.gr <- GenomicRanges::GRanges(
        seqnames = interact.DT$chr,
        ranges = IRanges::IRanges(
            start = interact.DT$End,
            end = interact.DT$End + 5000
        )
    )
    plac_end.gr <- echodata::dt_to_granges(dat = plac_end.gr,
                                           style = "UCSC",
                                           verbose = FALSE)
    #### find overlaps ####
    end_overlaps <- GenomicRanges::findOverlaps(
        query = consensus.gr,
        subject = plac_end.gr
    )
    start_overlaps <- GenomicRanges::findOverlaps(
        query = consensus.gr,
        subject = plac_start.gr
    )
    all_overlaps <- unique(c(
        S4Vectors::subjectHits(end_overlaps),
        S4Vectors::subjectHits(start_overlaps)
    )) 
    interact.DT$consensus_snp_overlap <- FALSE
    interact.DT$consensus_snp_overlap[all_overlaps] <- TRUE
    return(interact.DT)
}