NOTT_2019.prepare_peak_overlap <- function(merged_DT,
                                           snp_filter = "!is.na(SNP)",
                                           return_counts = TRUE) {
    PEAKS <- NOTT_2019.get_epigenomic_peaks()
    # Get SNP groups
    finemap_dat <- subset(merged_DT, eval(parse(text = snp_filter)),
        .drop = FALSE
    )
    # Get overlap with PEAKS
    gr.hits <- granges_overlap(
        dat1 = finemap_dat,
        chrom_col.1 = "CHR",
        start_col.1 = "POS",
        end_col.1 = "POS",
        dat2 = PEAKS
    )

    if (return_counts) {
        merged_annot <- find_topConsensus(
            dat = data.frame(gr.hits),
            grouping_vars = c(
                "Locus",
                "Cell_type",
                "Assay"
            )
        )
        dat_melt <- count_and_melt(
            merged_annot = merged_annot,
            snp_filter = snp_filter
        )
        if (sum(dat_melt$Count == 0 | is.na(dat_melt$Count), na.rm = T) > 0) {
            try({
                dat_melt[dat_melt$Count == 0 |
                    is.na(dat_melt$Count), "Count"] <- NA
            })
        }
        return(dat_melt)
    } else {
        return(gr.hits)
    }
}
