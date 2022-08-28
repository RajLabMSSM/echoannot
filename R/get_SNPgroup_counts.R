#' Get SNP group counts
#' 
#' Tally locus-specific SNP group sizes.
#' @param grouping_vars Column names in \code{merged_DT} to group counts by.
#' @param verbose Print messages.
#' @returns Named list of report tables.
#' @inheritParams super_summary_plot
#' 
#' @family summarise
#' @export
#' @importFrom dplyr group_by_at summarise n_distinct
#' @importFrom methods show
#' @importFrom data.table data.table
#' @examples
#' merged_DT <- echodata::get_Nalls2019_merged()
#' snp_groups <- get_SNPgroup_counts(merged_DT = merged_DT)
get_SNPgroup_counts <- function(merged_DT,
                                grouping_vars = "Locus",
                                verbose = TRUE) {
    SNP <- P <- Support <- Consensus_SNP <- mean.PP <- leadSNP <-
        Consensus <- NULL

    #### All loci ####
    snp_groups <-  
        merged_DT |>
            dplyr::group_by_at(.vars = grouping_vars) |>
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
                ),
                .groups = "keep" 
    ) |> data.table::data.table()
    messager("All loci",
             paste0("(",formatC(nrow(snp_groups),big.mark = ","),") : "),
             v=verbose)
    locus_summary <- 
        colSums(snp_groups[,-(grouping_vars), with=FALSE]) / 
        dplyr::n_distinct(snp_groups$Locus)
    methods::show(round(locus_summary,2))
    
    #### Only loci with consensus SNPs ####
    consensus_present <- subset(snp_groups, Consensus > 0)
    messager("Loci with at least one Consensus SNP",
             paste0("(",formatC(nrow(consensus_present),big.mark = ","),") : "),
             v=verbose)
    consensus_summary <- 
        colSums(consensus_present[,-(grouping_vars), with=FALSE]) /
        dplyr::n_distinct(consensus_present$Locus)
    methods::show(round(consensus_summary,2))
    #### Return ####
    return(list(snp_groups=snp_groups,
                locus_summary=locus_summary,
                consensus_summary=consensus_summary))
}
