#' Get overlap between SNPs and epigenomic peaks
#'
#' @keywords internal
#' @importFrom GenomicRanges GRanges
peak_overlap <- function(merged_DT,
                         snp_filter = "!is.na(SNP)",
                         include.NOTT2019_peaks = TRUE,
                         include.NOTT2019_enhancers_promoters = TRUE,
                         include.NOTT2019_PLACseq = TRUE,
                         include.CORCES2020_scATACpeaks = TRUE,
                         include.CORCES2020_Cicero_coaccess = TRUE,
                         include.CORCES2020_bulkATACpeaks = TRUE,
                         include.CORCES2020_HiChIP_FitHiChIP_coaccess = TRUE,
                         include.CORCES2020_gene_annotations = TRUE,
                         verbose = T) {
    gr.hits <- GenomicRanges::GRanges()
    ######## NOTT et al. 2019 #########
    if (include.NOTT2019_peaks) {
        try({
            NOTTpeaks <- NOTT2019_prepare_peak_overlap(
                merged_DT = merged_DT,
                snp_filter = snp_filter,
                return_counts = FALSE
            )
            NOTTpeaks <- clean_granges(NOTTpeaks)
            NOTTpeaks$Study <- "Nott et al. (2019)"
            gr.hits <- c(gr.hits, NOTTpeaks)
        })
    }

    if (include.NOTT2019_enhancers_promoters) {
        try({
            NOTTreg <- NOTT2019_prepare_regulatory_overlap(
                merged_DT = merged_DT,
                snp_filter = snp_filter,
                return_counts = FALSE
            )
            NOTTreg <- clean_granges(NOTTreg)
            NOTTreg$background <- 1
            NOTTreg$Study <- "Nott et al. (2019)"
            gr.hits <- c(gr.hits, NOTTreg)
        })
    }

    if (include.NOTT2019_PLACseq) {
        try({
            NOTTplac <- NOTT2019_prepare_placseq_overlap(
                merged_DT = merged_DT,
                snp_filter = snp_filter,
                return_counts = FALSE
            )
            NOTTplac <- clean_granges(NOTTplac)
            NOTTplac$background <- NA
            NOTTplac$Study <- "Nott et al. (2019)"
            gr.hits <- c(gr.hits, NOTTplac)
        })
    }

    ######## CORCES et al. 2020 #########
    if (include.CORCES2020_scATACpeaks) {
        try({
            CORCES_scPeaks <- CORCES2020_prepare_scATAC_peak_overlap(
                merged_DT = merged_DT,
                snp_filter = snp_filter,
                add_cicero = include.CORCES2020_Cicero_coaccess,
                annotate_genes = include.CORCES2020_gene_annotations,
                verbose = verbose,
                return_counts = FALSE
            )
            CORCES_scPeaks <- clean_granges(CORCES_scPeaks)
            CORCES_scPeaks$background <- NA
            CORCES_scPeaks$Study <- "Corces et al. (2020)"
            gr.hits <- c(gr.hits, CORCES_scPeaks)
        })
    }
    if (include.CORCES2020_bulkATACpeaks) {
        try({
            CORCES_bulkPeaks <- CORCES2020_prepare_bulkATAC_peak_overlap(
                merged_DT = merged_DT,
                snp_filter = snp_filter,
                add_HiChIP_FitHiChIP = include.CORCES2020_HiChIP_FitHiChIP_coaccess,
                annotate_genes = include.CORCES2020_gene_annotations,
                verbose = verbose,
                return_counts = FALSE
            )
            CORCES_bulkPeaks <- clean_granges(CORCES_bulkPeaks)
            CORCES_bulkPeaks$background <- NA
            CORCES_bulkPeaks$Study <- "Corces et al. (2020)"
            gr.hits <- c(gr.hits, CORCES_bulkPeaks)
        })
    }
    messager(length(gr.hits), "hits across",
        length(unique(gr.hits$Assay)), "assays in",
        length(unique(gr.hits$Study)), "studies found.",
        v = verbose
    )
    return(gr.hits)
}
