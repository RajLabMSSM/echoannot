#' Get overlap between datatable of SNPs and scATAC peaks
#'
#' Can optionally add \code{Cicero} coaccessibility scores,
#' which are also derived from scATAC-seq data.
#'
#' @family CORCES2020
#' @source \url{https://doi.org/10.1038/s41588-020-00721-x}
CORCES2020_get_ATAC_peak_overlap <- function(query_dat,
                                             FDR_filter = NULL,
                                             add_cicero = TRUE,
                                             cell_type_specific = TRUE,
                                             verbose = TRUE) {
    FDR <- Cicero <- NULL

    if (cell_type_specific) {
        messager("CORCES2020:: Extracting overlapping",
            "cell-type-specific scATAC-seq peaks",
            v = verbose
        )
        dat <- get_CORCES2020_scATACseq_celltype_peaks()
        Assay <- "scATAC"
    } else {
        messager("CORCES2020:: Extracting overlapping",
            "bulkATAC-seq peaks from brain tissue",
            v = verbose
        )
        dat <- get_CORCES2020_bulkATACseq_peaks()
        Assay <- "bulkATAC"
    }
    gr.peaks_lifted <- echotabix::liftover(
        sumstats_dt = dat,
        ref_genome = "hg38",
        convert_ref_genome = "hg19",
        chrom_col = "hg38_Chromosome",
        start_col = "hg38_Start",
        end_col = "hg38_Stop",
        as_granges = TRUE,
        style = "NCBI",
        verbose = FALSE
    )
    #### Get overlap with PEAKS ####
    gr.hits <- granges_overlap(
        dat1 = query_dat,
        chrom_col.1 = "CHR",
        start_col.1 = "POS",
        dat2 = gr.peaks_lifted
    )
    gr.hits$Assay <- Assay
    if (!is.null(FDR_filter)) {
        gr.hits <- subset(gr.hits, FDR < FDR_filter)
    }

    if (add_cicero & cell_type_specific) {
        try({
            # Pretty sure the Peak_IDs are shared between the
            # sc-ATACseq data and cicero,
            # because Cicero derives coaccess from sc-ATAC-seq data:
            # http://www.cell.com/molecular-cell/retrieve/pii/S1097276518305471?_returnURL=https%3A%2F%2Flinkinghub.elsevier.com%2Fretrieve%2Fpii%2FS1097276518305471%3Fshowall%3Dtrue
            ## Also pretty sure that checking for cicero overlap
            # only in the scATACseq gr.hits object is ok
            # bc you can only test for coaccessibility if there's a
            # peak to begin with.
            cicero <- get_CORCES2020_cicero_coaccessibility()
            cicero_dict <- c(
                stats::setNames(
                    cicero$Coaccessibility,
                    cicero$Peak_ID_Peak1
                ),
                stats::setNames(
                    cicero$Coaccessibility,
                    cicero$Peak_ID_Peak2
                )
            )
            if (any(!is.na(cicero_dict[gr.hits$Peak_ID]))) {
                gr.hits$Cicero <- cicero_dict[gr.hits$Peak_ID]
                gr.cicero <- subset(gr.hits, !is.na(Cicero))
                gr.cicero$Assay <- "Cicero"
                messager(
                    "+ CORCES2020:: Cicero coaccessibility scores",
                    "identified for",
                    length(gr.cicero), "/", length(gr.hits), "peak hits.",
                    v = verbose
                )
                gr.hits <- rbind_granges(
                    gr1 = gr.hits,
                    gr2 = gr.cicero
                )
            } else {
                messager("+ CORCES2020:: No Cicero hits found.", v = verbose)
            }
        })
    }
    if (cell_type_specific == FALSE) {
        gr.hits$brain <- 1
    }
    return(gr.hits)
}
