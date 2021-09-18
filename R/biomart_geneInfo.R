#' Get gene info using Biomart
#'
#' @keywords internal
#' @family annotate
#' @source
#' \href{https://bioconductor.org/packages/release/bioc/html/biomaRt.html}{
#' biomaRt}
#' @examples
#' \dontrun{
#' gene_info <- biomart_geneInfo(c("PTK2B", "CLU", "APOE"))
#' }
#' @importFrom biomaRt useMart getBM
biomart_geneInfo <- function(geneList,
                             reference_genome = "grch37") {
    # listDatasets(useMart("ENSEMBL_MART_ENSEMBL") )
    gene_mart <- biomaRt::useMart("ENSEMBL_MART_ENSEMBL",
        dataset = "hsapiens_gene_ensembl",
        host = paste0(reference_genome, ".ensembl.org")
    )
    # View(listFilters(gene_mart))
    # View(listAttributes(gene_mart))
    gene_results <- biomaRt::getBM(
        mart = gene_mart,
        filters = "hgnc_symbol",
        # values = unlist(strsplit(snp_results$ensembl, ";")),
        values = geneList,
        attributes = c(
            "hgnc_symbol", "external_gene_name", "ensembl_gene_id",
            "chromosome_name", "start_position", "end_position"
        )
    )
    return(gene_results)
}
