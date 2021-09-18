NOTT_2019.prepare_placseq_overlap <- function(merged_DT,
                                              snp_filter = "!is.na(SNP)",
                                              return_counts = TRUE) {
    finemap_dat <- subset(merged_DT, eval(parse(text = snp_filter)),
        .drop = FALSE
    )

    if (return_counts) {
        interactome <- NOTT_2019.get_interactions(finemap_dat = finemap_dat)
        dat_melt <- count_and_melt(
            merged_annot = interactome,
            snp_filter = snp_filter
        )
        if (sum(dat_melt$Count == 0 | is.na(dat_melt$Count),
            na.rm = TRUE
        ) > 0) {
            try({
                dat_melt[dat_melt$Count == 0 |
                    is.na(dat_melt$Count), "Count"] <- NA
            })
        }
        return(dat_melt)
    } else {
        interactome <- NOTT_2019.get_interactions(
            finemap_dat = finemap_dat,
            as.granges = TRUE
        )
        return(interactome)
    }
}
