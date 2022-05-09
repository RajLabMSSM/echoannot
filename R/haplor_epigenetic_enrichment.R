#' Test for enrichment of \code{HaploR} annotations
#'
#' @keywords internal
#' @importFrom methods show
#' @family annotate
#' @source
#' \href{https://cran.r-project.org/web/packages/haploR/vignettes/haplor-vignette.html}{
#' HaploR}
haplor_epigenetics_enrichment <- function(snp_list1,
                                          snp_list2,
                                          chisq = TRUE,
                                          fisher = TRUE,
                                          epigenetic_variables =
                                              c(
                                                  "Promoter_histone_marks",
                                                  "Enhancer_histone_marks"
                                              ),
                                          tissue_list = c("BRN", "BLD")) {
    .SD <- NULL

    messager("Conducting SNP epigenomic annotation enrichment tests...")
    # Foreground
    messager("+++ SNP list 1 :")
    HR1 <- haplor_haploreg(snp_list1)
    summ1 <- haplor_epigenetics_summary(HR1, tissue_list = tissue_list)
    # Background
    messager("+++ SNP list 2 :")
    HR2 <- haplor_haploreg(snp_list2)
    summ2 <- haplor_epigenetics_summary(HR2, tissue_list = tissue_list)

    for (epi_var in epigenetic_variables) {
        messager(
            "++ Testing for enrichment of '", epi_var,
            "' in the tissues '", paste(tissue_list, collapse = " & "), "'"
        )
        # Create contingency table
        cont_tab <- rbind(
            list1 = summ1[epi_var, c("Hits", "Total_SNPs")] %>%
                unlist(),
            list2 = summ2[epi_var, c("Hits", "Total_SNPs")] %>%
                unlist()
        ) %>% as.table()
        # Conduct tests
        if (chisq) {
            chisq.results <- stats::chisq.test(cont_tab,
                simulate.p.value = TRUE
            )
            methods::show(chisq.results)
        }
        if (fisher) {
            fisher.results <- stats::fisher.test(cont_tab)
            methods::show(fisher.results)
        }
    }
}
