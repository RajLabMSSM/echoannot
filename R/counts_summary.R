counts_summary <- function(top_SNPs,
                           merged_results,
                           verbose = TRUE) {
    Dataset <- Locus <- count <- Support <- SNP <- NULL

    # Get total # of SNPs per gene per dataset
    candidate_counts <- merged_results %>%
        dplyr::group_by(Dataset, Locus) %>%
        count(name = "Total_SNPs")
    max_consensus <- sum(endsWith(colnames(merged_results), ".CS"))
    candidate_counts <- merge.data.frame(candidate_counts,
        dplyr::group_by(merged_results, Locus) %>%
            count(name = "CS"),
        by = "Locus", all = TRUE
    )
    candidate_counts <- merge.data.frame(candidate_counts,
        subset(merged_results, Support == max_consensus) %>%
            dplyr::group_by(Locus) %>%
            count(name = "Consensus_SNPs"),
        all = TRUE
    )
    # Add lead rsid column
    candidate_counts <- merge.data.frame(candidate_counts,
        top_SNPs[, c("Locus", "SNP")] %>%
            dplyr::rename(leadSNP = SNP),
        on = "Locus", all = TRUE
    )
    # Add "is lead SNP in Credible Set" column
    candidate_counts <- merge.data.frame(candidate_counts,
        leadSNP_comparison(top_SNPs, merged_results),
        on = "Locus", all = TRUE
    )
    # Gather credible set rsids
    CredSet_rsids <- merged_results %>%
        dplyr::group_by(Dataset, Locus) %>%
        subset(Support == max_consensus) %>%
        dplyr::summarise(CredSet_rsids = paste(SNP, collapse = "; ")) %>%
        data.table::data.table()
    candidate_counts <- merge.data.frame(candidate_counts,
        CredSet_rsids,
        on = "Locus", all = TRUE
    )
    # Gather consensus rsids
    consensus_rsids <- merged_results %>%
        dplyr::group_by(Dataset, Locus) %>%
        subset(Support == T) %>%
        dplyr::summarise(Consensus_rsids = paste(SNP, collapse = "; ")) %>%
        data.table::data.table()
    candidate_counts <- merge.data.frame(candidate_counts,
        consensus_rsids,
        on = "Locus", all = TRUE
    )
    # Fill 0s
    candidate_counts$Consensus_SNPs[is.na(candidate_counts$Consensus_SNPs)] <- 0
    means <- c(
        Locus = " ",
        Dataset = "[Average]",
        candidate_counts[, c("Total_SNPs", "CS", "Consensus_SNPs")] %>% 
            colMeans() %>% round(1),
        leadSNP_in_CredSet = paste0(
            round(sum(candidate_counts$leadSNP_in_CredSet) /
                      dim(candidate_counts)[1] * 100, 2),
            "%"),
        CredSet_rsids = "",
        Conensus_rsids = ""
    )
    # Add averages
    candidate_counts <- suppressWarnings(rbind(candidate_counts, means))
    percent_model_convergence <- round(
        sum(candidate_counts$Consensus_SNPs > 0) /
            length(candidate_counts$Consensus_SNPs) * 100, 2)
    max_consensus_set <- max(candidate_counts$Consensus_SNPs)
    # Check if lead SNP is in the credible sets for each locus
    messager("\n + All", max_consensus, "models converged upon 1 to",
        max_consensus_set, "SNPs in", percent_model_convergence, "% of loci.",
        v = verbose
    ) 
    return(candidate_counts)
}
