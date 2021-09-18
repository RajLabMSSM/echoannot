#' Download SNP-wise annotations from Biomart
#'
#' @keywords internal
#' @family annotate
#' @source
#' \href{https://bioconductor.org/packages/release/bioc/html/biomaRt.html}{
#' biomaRt}
#' @importFrom biomaRt useMart getBM
#' @importFrom data.table as.data.table
biomart_snp_info <- function(snp_list,
                             reference_genome = "grch37",
                             attributes = c(
                                 "refsnp_id",
                                 "allele",
                                 "chr_name",
                                 "chrom_start",
                                 "chrom_end",
                                 "chrom_strand",
                                 "ensembl_gene_stable_id",
                                 "consequence_type_tv",
                                 "polyphen_prediction",
                                 "polyphen_score",
                                 "sift_prediction",
                                 "sift_score",
                                 "reg_consequence_types",
                                 "validated"
                             ),
                             verbose = TRUE) {
    # library(biomaRt)
    messager("+ Gathering annotation data from Biomart...", v = verbose)
    mart <- biomaRt::useMart(
        biomart = "ENSEMBL_MART_SNP",
        host = paste0(reference_genome, ".ensembl.org"),
        path = "/biomart/martservice",
        dataset = "hsapiens_snp"
    )
    # View(biomaRt::listFilters(mart))
    # View(biomaRt::listAttributes(mart))
    biomart_query <- tryCatch(
        {
            biomaRt::getBM(
                attributes = attributes,
                filters = c("snp_filter"),
                values = unique(snp_list),
                mart = mart
            )
        },
        error = function(e) {
            message(e)
            message("Retrying with `useCache=FALSE`")
            biomaRt::getBM(
                attributes = attributes,
                filters = c("snp_filter"),
                values = unique(snp_list),
                mart = mart,
                # Important! sometimes biomart will use an old cache
                ## that doesn't work with the current version of biomart.
                useCache = FALSE
            )
        }
    )
    biomart_query <- data.table::as.data.table(biomart_query)
    biomart_query[
        biomart_query$consequence_type_tv == "",
    ]$consequence_type_tv <- NA
    # Only take the first annotation per variant
    # annotated_results %>% dplyr::group_by(Dataset, Gene, SNP) %>% slice(1)
    return(biomart_query)
}
