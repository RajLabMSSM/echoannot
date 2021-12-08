#' Conduct enrichment tests for each annotation
#'
#' XGR uses a binomial enrichment tests for each annotation.
#'
#'
#' \href{https://www.rdocumentation.org/packages/XGR/versions/1.1.5/topics/xDefineGenomicAnno}{
#' Description of all datasets}
#' @inheritParams XGR_prepare_foreground_background
#' @examples
#' \dontrun{
#' enrich_res <- XGR_iterate_enrichment(
#'     dat = echodata::get_Nalls2019_merged(),
#'     foreground_filter = "Consensus_SNP",
#'     background_filter = "leadSNP",
#'     lib.selections = c("ENCODE_TFBS_ClusteredV3_CellTypes")
#' )
#' }
#' @family XGR
#' @keywords internal
#' @importFrom data.table rbindlist data.table fwrite
#' @importFrom parallel mclapply
#' @importFrom XGR xGRviaGenomicAnno xRDataLoader
#' @importFrom stats p.adjust
#' @importFrom dplyr %>% mutate arrange
XGR_iterate_enrichment <- function(dat,
                                   foreground_filter = "Consensus_SNP",
                                   background_filter = "leadSNP",
                                   lib.selections =
                                       c(
                                           "ENCODE_TFBS_ClusteredV3_CellTypes",
                                           "ENCODE_DNaseI_ClusteredV3_CellTypes",
                                           "Broad_Histone",
                                           "FANTOM5_Enhancer",
                                           "Segment_Combined_Gm12878",
                                           "TFBS_Conserved",
                                           "ReMap_PublicAndEncode_TFBS",
                                           "Blueprint_VenousBlood_Histone",
                                           "Blueprint_DNaseI",
                                           # "Blueprint_Methylation_hyper",
                                           # "Blueprint_Methylation_hypo",
                                           # "Genic_anno",
                                           "FANTOM5_CAT_Cell",
                                           "FANTOM5_CAT_MESH",
                                           "GWAScatalog_alltraits"
                                       ),
                                   save_path = FALSE,
                                   nThread = 1) {
    
    FDR <- nOverlap <- fc <- adjp <- pvalue <- NULL;
    fg_bg <- XGR_prepare_foreground_background(
        dat,
        foreground_filter = foreground_filter,
        background_filter = background_filter
    )

    # lib.selections = c("ENCODE_TFBS_ClusteredV3_CellTypes",
    #                    "ENCODE_DNaseI_ClusteredV3_CellTypes",
    #                    "Broad_Histone",
    #                    "UW_Histone",
    #                    "SYDH_Histone",
    #                    "FANTOM5_Enhancer",
    #                    "TFBS_Conserved",
    #                    "Uniform_TFBS",
    #                    "Uniform_DNaseI_HS")
    # roadmap_grl <- lapply(unique(dat$Locus), function(locus){
    #       locus_DT <- subset(dat, Locus==locus)
    #       dat <- ROADMAP_tabix(results_path=results_path,
    #                            chrom = locus_DT$CHR[1],
    #                            min_pos = min(locus_DT$POS),
    #                            max_pos = max(locus_DT$POS),
    #                            eid=eid,
    #                            convert_to_granges=T)
    #       return(dat)
    # })
    database_results <- parallel::mclapply(lib.selections, function(lib.name) {
        messager("XGR:: Testing enrichment: ", lib.name)
        eTerm <- NULL
        try({
            GR.annotations <- XGR::xRDataLoader(RData.customised = lib.name)
            eTerm <- lapply(GR.annotations, function(grl) {
                et <- XGR::xGRviaGenomicAnno(
                    data.file = fg_bg$foreground,
                    background.file = fg_bg$background,
                    format.file = "data.frame",
                    GR.annotation = grl
                )
                return(et)
            }) %>% data.table::rbindlist()
            eTerm$lib <- lib.name
            eTerm$fullname <- names(unlist(GR.annotations))
            eTerm$source <- lapply(
                eTerm$fullname,
                function(e) {
                    strsplit(e, "[.]")[[1]][1]
                }
            ) %>%
                as.character()
            eTerm$assay <- lapply(
                eTerm$fullname,
                function(e) {
                    strsplit(e, "[.]")[[1]][2]
                }
            ) %>%
                as.character()
        })
        return(eTerm)
    }, mc.cores = nThread)

    # Re-calculate corrected p-val to account for multiple dbs tested
    enrich_res <- data.table::rbindlist(database_results) %>%
        dplyr::mutate(
            FDR = stats::p.adjust(
                p = pvalue,
                method = "fdr"
            ),
            Bonf = stats::p.adjust(
                p = pvalue,
                method = "bonferroni"
            )
        ) %>%
        dplyr::arrange(FDR, -nOverlap, -fc) %>%
        subset(adjp < 0.05) %>%
        data.table::data.table()
    if (save_path != FALSE) {
        data.table::fwrite(enrich_res, save_path, quote = FALSE)
    }
    return(enrich_res)
}
