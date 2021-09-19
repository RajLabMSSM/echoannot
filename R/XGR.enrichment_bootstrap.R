#' XGR enrichment (bootstrapped)
#'
#' Perform annotation enrichment tests using iterative bootstrapping procedure.
#'
#' @param snp_groups Which SNP groups to repeat enrichment tests for separately.
#' @param iterations Number of bootstrapping iterations.
#' @param bootstrap Whether to use bootstrapping.
#' @param save_path File path to save results to.
#' @param nThread Number of threads to parallelise bootstrapping over.
#' @inheritParams XGR.enrichment
#'
#' @family XGR
#' @examples
#' \dontrun{
#' gr.merged <- echoannot::merge_celltype_specific_epigenomics()
#' enrich_res <- XGR.enrichment_bootstrap(
#'     gr = gr.merged,
#'     merged_dat = echodata::get_Nalls2019_merged()
#' )
#' }
#' @importFrom data.table rbindlist fwrite
#' @importFrom parallel mclapply
#' @importFrom dplyr %>%
#' @importFrom stats p.adjust
XGR.enrichment_bootstrap <- function(gr,
                                     merged_dat,
                                     snp_groups = c(
                                         "Random",
                                         "GWAS lead",
                                         "UCS (-PolyFun)",
                                         "UCS", "Consensus (-PolyFun)",
                                         "Consensus"
                                     ),
                                     background_filter = NULL,
                                     grouping_vars = c(
                                         "Study",
                                         "Assay",
                                         "Cell_type"
                                     ),
                                     iterations = 1000,
                                     fg_sample_size = 20,
                                     bg_sample_size = NULL,
                                     bootstrap = TRUE,
                                     save_path = tempfile(
                                         fileext =
                                             "XGR_enrich_boot_res.csv.gz"
                                     ),
                                     nThread = 1,
                                     verbose = TRUE) {
    if (bootstrap) {
        messager("XGR:: Initiating bootstrap enrichment procedure", v = verbose)
    } else {
        iterations <- 1
        bg_sample_size <- NULL
        fg_sample_size <- NULL
    }
    sampling_df <- merged_dat

    RES_GROUPS <- lapply(
        snp_groups,
        function(snp_group,
                 .merged_dat = merged_dat,
                 .grouping_vars = grouping_vars,
                 .background_filter = background_filter,
                 .fg_sample_size = fg_sample_size,
                 .bg_sample_size = bg_sample_size) {
            snp_filters <- snp_group_filters(random_sample_size = .fg_sample_size)
            .foreground_filter <- snp_filters[snp_group]
            message(snp_group, " :: ", .foreground_filter)
            RES <- parallel::mclapply(seq(1, iterations),
                function(i,
                         merged_dat = .merged_dat,
                         grouping_vars = .grouping_vars,
                         foreground_filter = .foreground_filter,
                         background_filter = .background_filter,
                         fg_sample_size = .fg_sample_size,
                         bg_sample_size = .bg_sample_size) {
                    try({
                        XGR.enrichment(
                            gr = gr,
                            merged_dat = merged_dat,
                            foreground_filter = foreground_filter,
                            background_filter = background_filter,
                            grouping_vars = grouping_vars,
                            fg_sample_size = fg_sample_size,
                            bg_sample_size = bg_sample_size
                        )
                    })
                },
                mc.cores = nThread
            ) %>% data.table::rbindlist(fill = TRUE)
            RES$SNP_group <- snp_group
            return(RES)
        }
    ) %>% data.table::rbindlist(fill = T)

    # Post-process
    RES_GROUPS <- RES_GROUPS %>%
        dplyr::mutate(
            SNP_group = factor(SNP_group,
                levels = unique(SNP_group),
                ordered = TRUE
            ),
            FDR = stats::p.adjust(p = pvalue, method = "fdr")
        )
    if (save_path != FALSE) {
        messager("XGR:: Saving enrichment results ==>", save_path, v = verbose)
        data.table::fwrite(RES_GROUPS, save_path)
    }
    return(RES_GROUPS)
}
