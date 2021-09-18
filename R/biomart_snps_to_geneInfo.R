#' Identify which genes SNPs belong to using Biomart
#'
#' @family annotate
#' @source
#' \href{https://bioconductor.org/packages/release/bioc/html/biomaRt.html}{
#' biomaRt}
#'
#' @keywords internal
#' @importFrom biomaRt useMart getBM
#' @importFrom dplyr %>%
#'
#' @examples
#' \dontrun{
#' # biomart_snps_to_geneInfo(c("rs114360492"))
#' }
biomart_snps_to_geneInfo <- function(snp_list,
                                     reference_genome = "grch37") {
    ensembl_gene_stable_id <- ensembl <- NULL
    # listMarts()
    snp_mart <- biomaRt::useMart("ENSEMBL_MART_SNP",
        dataset = "hsapiens_snp",
        host = paste0(reference_genome, ".ensembl.org")
    )
    # View(listFilters(snp_mart))
    # View(listAttributes(snp_mart))
    snp_results <- biomaRt::getBM(snp_mart,
        filters = "snp_filter",
        values = snp_list,
        attributes = c(
            "refsnp_id", "snp",
            "chr_name",
            "chrom_start",
            "chrom_end",
            "associated_gene",
            "ensembl_gene_stable_id"
        )
    )
    # # Split ensembl IDs
    gene_mart <- biomaRt::useMart("ENSEMBL_MART_ENSEMBL",
        dataset = "hsapiens_gene_ensembl"
    )
    gene_results <- biomaRt::getBM(
        mart = gene_mart,
        filters = "ensembl_gene_id",
        # values = unlist(strsplit(snp_results$ensembl, ";")),
        values = snp_results$ensembl_gene_stable_id,
        attributes = c(
            "hgnc_symbol", "external_gene_name", "ensembl_gene_id",
            "chromosome_name", "start_position", "end_position"
        )
    )
    snp_results <- snp_results %>%
        mutate(ensembl = strsplit(
            as.character(ensembl_gene_stable_id), ";"
        )) %>%
        tidyr::unnest(ensembl)
    merged_df <- data.table(gene_results,
        key = "ensembl_gene_id"
    )[data.table(
        snp_results,
        key = "ensembl"
    )]
    return(merged_df)
}
