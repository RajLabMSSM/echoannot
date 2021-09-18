#' Tally locus-specific SNP group sizes
#'
#' @family summarise
#' @examples
#' data("merged_DT")
#' snp_groups <- get_SNPgroup_counts(merged_DT = echodata::Nalls2019_merged)
#' @importFrom dplyr %>%
get_SNPgroup_counts <- function(merged_DT,
                                grouping_vars = "Locus") {
    SNP <- P <- Support <- Consensus_SNP <- mean.PP <- leadSNP <-
        Consensus <- NULL

    snp_groups <- suppressMessages(
        merged_DT %>%
            dplyr::group_by(.dots = grouping_vars) %>%
            dplyr::summarise(
                Total.SNPs = dplyr::n_distinct(SNP,
                    na.rm = TRUE
                ),
                nom.sig.GWAS = dplyr::n_distinct(SNP[P < .05],
                    na.rm = TRUE
                ),
                sig.GWAS = dplyr::n_distinct(SNP[P < 5e-8],
                    na.rm = TRUE
                ),
                CS = dplyr::n_distinct(SNP[Support > 0],
                    na.rm = TRUE
                ),
                Consensus = dplyr::n_distinct(SNP[Consensus_SNP],
                    na.rm = TRUE
                ),
                topConsensus = dplyr::n_distinct(SNP[Consensus_SNP &
                    mean.PP == max(mean.PP)],
                na.rm = TRUE
                ),
                topConsensus.leadGWAS = dplyr::n_distinct(
                    SNP[Consensus_SNP & leadSNP],
                    na.rm = TRUE
                )
            )
    )
    message("Report:: all loci:")
    print(snp_groups[, !colnames(snp_groups) %in% grouping_vars] %>%
        colSums() / dplyr::n_distinct(snp_groups$Locus))
    message("Report:: loci with at least one Consensus SNP:")
    consensus_present <- subset(snp_groups, Consensus > 0)
    print(consensus_present[, !colnames(consensus_present) %in%
        grouping_vars] %>%
        colSums() / dplyr::n_distinct(consensus_present$Locus))
    return(data.frame(snp_groups))
}
