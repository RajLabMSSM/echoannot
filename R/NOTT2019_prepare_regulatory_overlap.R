NOTT2019_prepare_regulatory_overlap <- function(merged_DT,
                                                snp_filter = "!is.na(SNP)",
                                                return_counts = TRUE) {
    Element <- NULL;
    gr.reg <- NOTT2019_get_regulatory_regions(as.granges = TRUE)
    finemap_sub <- subset(merged_DT, eval(parse(text = snp_filter)), 
                          .drop = FALSE)
    gr.hits.reg <- granges_overlap(
        dat1 = finemap_sub,
        chrom_col.1 = "CHR",
        start_col.1 = "POS",
        end_col.1 = "POS",
        dat2 = gr.reg
    )
    if (return_counts) {
        merged_annot.reg <- find_top_consensus(
            dat = data.frame(gr.hits.reg) %>% dplyr::rename(Assay = Element),
            grouping_vars = c("Locus", "Cell_type", "Assay")
        )
        dat_melt.reg <- count_and_melt(
            merged_annot = merged_annot.reg,
            snp_filter = snp_filter
        )
        return(dat_melt.reg)
    } else {
        GenomicRanges::mcols(gr.hits.reg)["Assay"] <- gr.hits.reg$Element
        return(gr.hits.reg)
    }
}
