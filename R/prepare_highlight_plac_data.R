prepare_highlight_plac_data <- function(dat,
                                        interact.DT,
                                        snp_filter="Support>0",
                                        anchor_size=5000,
                                        verbose = TRUE){
    
    messager("Preparing data for highlighting PLAC-seq interactions",
             "that overlap with SNP subset:",snp_filter,
             v=verbose)
    target_snps <- subset(dat, eval(parse(text = snp_filter)))
    if(nrow(target_snps)==0){
        stp <- "No target SNPs overlapped with PLAC-seq anchors."
        stop(stp)
    }  
    #### Convert to UCSC format ####
    target.gr <- echodata::dt_to_granges(dat = target_snps,
                                         style = "UCSC", 
                                         verbose = FALSE) 
    plac_start.gr <- GenomicRanges::GRanges(
        seqnames = interact.DT$chr,
        ranges = IRanges::IRanges(
            start = interact.DT$Start - (anchor_size/2),
            end = interact.DT$Start + (anchor_size/2)
        )
    )
    plac_start.gr <- echodata::dt_to_granges(dat = plac_start.gr,
                                             style = "UCSC",
                                             verbose = FALSE)
    plac_end.gr <- GenomicRanges::GRanges(
        seqnames = interact.DT$chr,
        ranges = IRanges::IRanges(
            start = interact.DT$End - (anchor_size/2),
            end = interact.DT$End + (anchor_size/2)
        )
    )
    plac_end.gr <- echodata::dt_to_granges(dat = plac_end.gr,
                                           style = "UCSC",
                                           verbose = FALSE)
    #### find overlaps ####
    end_overlaps <- GenomicRanges::findOverlaps(
        query = target.gr,
        subject = plac_end.gr
    )
    start_overlaps <- GenomicRanges::findOverlaps(
        query = target.gr,
        subject = plac_start.gr
    )
    all_overlaps <- unique(c(
        S4Vectors::subjectHits(end_overlaps),
        S4Vectors::subjectHits(start_overlaps)
    )) 
    interact.DT$target_snp_overlap <- FALSE
    interact.DT$target_snp_overlap[all_overlaps] <- TRUE
    return(interact.DT)
}
