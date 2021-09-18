#' Give a quick summary report of the fine-mapping results
#' @family summarise
#' @examples
#' \dontrun{
#' results_report(echodata::Nalls2019_merged)
#' }
#' @return Null
#' @keywords internal
results_report <- function(merged_dat) {
    leadSNP <- Support <- Consensus_SNP <- NULL

    message("echolocatoR results report (all loci):")
    messager("+ Overall report:")
    messager("++", length(unique(merged_dat$Locus)), "Loci.")
    messager("++", nrow(merged_dat), "SNPs.")
    cat("\n")
    messager("+ Lead SNP report:")
    messager("++", length(subset(merged_dat, leadSNP)$SNP), "lead SNPs.")
    messager(
        "++ Lead SNP mean PP =",
        round(mean(subset(merged_dat, leadSNP)$mean.PP, na.rm = TRUE), 2)
    )
    cat("\n")
    messager("+ Union Credible Set report:")
    messager("++", length(subset(merged_dat, Support > 0)$SNP), "UCS SNPs.")
    messager(
        "++ UCS mean PP =",
        round(mean(subset(merged_dat, Support > 0)$mean.PP, na.rm = TRUE), 2)
    )
    messager(
        "++", nrow(subset(merged_dat, Support > 0 & leadSNP)),
        "UCS SNPs that are also lead SNPs"
    )
    cat("\n")
    messager("+ Consensus SNP report:")
    messager("++", length(subset(
        merged_dat,
        Consensus_SNP
    )$SNP), "Consensus SNPs.")
    messager(
        "++ Consensus SNP mean PP =",
        round(mean(subset(
            merged_dat,
            Consensus_SNP
        )$mean.PP, na.rm = TRUE), 2)
    )
    messager(
        "++", nrow(subset(merged_dat, Consensus_SNP & leadSNP)),
        "Consensus SNPs that are also lead SNPs"
    )
}
