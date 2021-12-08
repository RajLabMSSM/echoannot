leadSNP_comparison <- function(top_SNPs,
                               merged_results) {
    leadSNP <- finemappedSNP <- Overlap <-
        leadSNP_summary_table <- data.table::merge.data.table(
            top_SNPs %>% dplyr::select(leadSNP = SNP, Gene),
            merged_results %>% dplyr::select(finemappedSNP = SNP, Gene),
            by = "Gene", all = T
        ) %>%
        dplyr::group_by(Gene, leadSNP) %>%
        dplyr::mutate(Overlap = leadSNP %in% finemappedSNP) %>%
        dplyr::group_by(Gene) %>%
        dplyr::summarise(Overlap = sum(Overlap)) %>%
        dplyr::mutate(leadSNP_in_CredSet = Overlap > 0)

    percent_leadSNPs <- round(
        sum(leadSNP_summary_table$leadSNP_in_CredSet) /
            length(leadSNP_summary_table$leadSNP_in_CredSet) * 100, 2
    )
    return(leadSNP_summary_table[, c("Gene", "leadSNP_in_CredSet")])
}
