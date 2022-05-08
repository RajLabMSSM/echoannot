#' Prepare data to plot overlap between datatable of SNPs and
#' cell-type-specific epigenomic peaks and coaccessibility data.
#'
#' @family CORCES2020
#' @source \url{https://doi.org/10.1038/s41588-020-00721-x}
#' @examples
#' \dontrun{
#' dat_melt <- echoannot:::CORCES2020_prepare_bulkATAC_peak_overlap(
#'     merged_DT = echodata::get_Nalls2019_merged()
#' )
#' }
#' @keywords internal
#' @importFrom GenomicRanges mcols
#' @importFrom data.table melt.data.table data.table
#' @importFrom stats setNames
#' @importFrom echodata get_CS_counts
CORCES2020_prepare_bulkATAC_peak_overlap <- function(
    merged_DT,
    FDR_filter = NULL,
    snp_filter = "Consensus_SNP==TRUE",
    add_HiChIP_FitHiChIP = TRUE,
    annotate_genes = FALSE,
    return_counts = TRUE,
    verbose = TRUE) {
    
    Peak_ID <- Cell_type <- Count <- NULL;
    #### Get SNP groups  ####
    query_dat <- subset(merged_DT, eval(parse(text = snp_filter)),
        .drop = FALSE
    )
    # Get overlap with PEAKS and merge
    gr.hits <- CORCES2020_get_ATAC_peak_overlap(
        query_dat = query_dat,
        add_cicero = FALSE,
        cell_type_specific = FALSE,
        verbose = verbose
    )
    annot_cols <- NULL;
    if (annotate_genes) {
        annot_cols <- c("Gene_Symbol", "CTCF", 
                        "Distance_To_TSS", "Annotation")
        messager("CORCES2020:: Annotating peaks by bulk brain target genes",
            v = verbose
        )
        peak_overlap <- subset(
            get_CORCES2020_bulkATACseq_peaks(),
            Peak_ID %in% unique(gr.hits$Peak_ID)
        )
        prefix <- ""
        for (column in annot_cols) {
            dict <- stats::setNames(peak_overlap[[column]],
                                    peak_overlap$Peak_ID)
            GenomicRanges::mcols(gr.hits)[[paste0(prefix, column)]] <-
                dict[gr.hits$Peak_ID]
        }
        annot_cols <- paste0(prefix, annot_cols)
    }

    if (add_HiChIP_FitHiChIP) {
        gr.anchor_hits <- CORCES2020_get_hichip_fithichip_overlap(
            query_dat = query_dat,
            verbose = verbose
        )
        gr.hits <- c(gr.hits, gr.anchor_hits)
    }

    #### Melt cell type into one col ####
    cell_melt <- data.table::melt.data.table(
        data.table::data.table(data.frame(gr.hits)),
        measure.vars = c("brain"),
        variable.name = "cell_name",
        value.name = "cell_value"
    )
    cell_melt[cell_melt$cell_value == 1, "Cell_type"] <-
        cell_melt[cell_melt$cell_value == 1, "cell_name"]
    cell_melt <- subset(cell_melt, !is.na(Cell_type), .drop = FALSE)

    if (return_counts) {
        dat_melt <- count_and_melt(
            merged_annot = cell_melt,
            grouping_vars = c(
                "Locus",
                "Cell_type",
                "Assay",
                annot_cols
            ),
            snp_filter = snp_filter
        )
        if (sum(dat_melt$Count == 0 | 
                is.na(dat_melt$Count), na.rm = TRUE) > 0) {
            try({
                dat_melt[dat_melt$Count == 0 |
                    is.na(dat_melt$Count), "Count"] <- NA
            })
        }

        dat_melt <- subset(dat_melt, !is.na(Count))
        dat_melt$background <- NA

        # Make sure locus order kept
        locus_order <- echodata::get_CS_counts(merged_DT)
        dat_melt$Locus <- factor(dat_melt$Locus,
            levels = locus_order$Locus,
            ordered = TRUE
        )
        return(dat_melt)
    } else {
        gr.melt <- echodata::dt_to_granges(
            dat = cell_melt, 
            chrom_col = "seqnames",
            start_col = "start",
            end_col = "end") 
        return(gr.melt)
    }
}
