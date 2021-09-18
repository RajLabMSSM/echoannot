#' Return only the missense SNPs
#'
#' @keywords internal
#' @family annotate
#' @source
#' \href{https://bioconductor.org/packages/release/bioc/html/biomaRt.html}{
#' biomaRt}
#' @importFrom dplyr %>% group_by select
snps_by_mutation_type <- function(merged_results,
                                  mutation_type = "missense_variant") {
    consequence_type_tv <- Dataset <- Gene <- SNP <- NULL

    potential_missense <- subset(
        merged_results,
        consequence_type_tv == mutation_type
    ) %>%
        dplyr::group_by(Dataset, Gene, SNP) %>%
        dplyr::select(
            Dataset, Gene, SNP,
            consequence_type_tv
        ) %>%
        unique()
    potential_missense_full <- subset(
        merged_results,
        SNP %in% potential_missense$SNP
    ) %>%
        dplyr::group_by(Dataset, Gene, SNP) %>%
        dplyr::select(
            Dataset, Gene, SNP,
            consequence_type_tv
        ) %>%
        unique()
    return(potential_missense_full)
}
