#' Prepare data to plot overlap between datatable of SNPs and
#' cell-type-specific epigenomic peaks and coaccessibility data.
#'
#' @family CORCES_2020
#' @source \url{https://doi.org/10.1038/s41588-020-00721-x}
#' @examples
#' \dontrun{
#' dat_melt <- CORCES_2020.prepare_scATAC_peak_overlap(
#'     merged_DT = echodata::Nalls2019_merged
#' )
#' }
#' @keywords internal
#' @importFrom GenomicRanges makeGRangesFromDataFrame
CORCES_2020.prepare_scATAC_peak_overlap <- function(merged_DT,
                                                    FDR_filter = NULL,
                                                    snp_filter =
                                                        "Consensus_SNP==TRUE",
                                                    add_cicero = TRUE,
                                                    annotate_genes = TRUE,
                                                    return_counts = TRUE,
                                                    verbose = TRUE) {
    Peak_ID <- Cell_type <- Count <- NULL

    cell_dict <- c(
        ExcitatoryNeurons = "neurons (+)",
        InhibitoryNeurons = "neurons (-)",
        NigralNeurons = "neurons (nigral)",
        Microglia = "microglia",
        Oligodendrocytes = "oligo",
        Astrocytes = "astrocytes",
        OPCs = "OPCs"
    )
    # Get SNP groups
    # finemap_dat[eval(parse(text=snp_filter))]
    finemap_dat <- subset(merged_DT, eval(parse(text = snp_filter)), .drop = FALSE)
    # Get overlap with PEAKS and merge
    gr.hits <- CORCES_2020.get_ATAC_peak_overlap(
        finemap_dat = finemap_dat,
        add_cicero = add_cicero,
        cell_type_specific = TRUE,
        verbose = verbose
    )

    annot_cols <- NULL
    if (annotate_genes & length(gr.hits) > 0) {
        annot_cols <- c("Gene_Symbol", "CTCF", "Distance_To_TSS", "Annotation")
        messager(
            "CORCES_2020:: Annotating peaks by cell-type-specific target genes",
            v = verbose
        )
        peak_overlap <- subset(
            echoannot::CORCES_2020.scATACseq_peaks,
            Peak_ID %in% unique(gr.hits$Peak_ID)
        )
        prefix <- ""
        for (column in annot_cols) {
            dict <- setNames(peak_overlap[[column]], peak_overlap$Peak_ID)
            GenomicRanges::mcols(gr.hits)[[paste0(prefix, column)]] <-
                dict[gr.hits$Peak_ID]
        }
        annot_cols <- paste0(prefix, annot_cols)
    }

    # Melt cell type into one col
    cell_melt <- data.table::melt.data.table(
        data.table::data.table(data.frame(gr.hits)),
        measure.vars = names(cell_dict),
        variable.name = "cell_name",
        value.name = "cell_value"
    )
    cell_melt[cell_melt$cell_value == 1, "Cell_type"] <-
        cell_melt[cell_melt$cell_value == 1, "cell_name"]
    cell_melt <- subset(cell_melt, !is.na(Cell_type), .drop = FALSE)

    if (return_counts) {
        dat_melt <- count_and_melt(
            merged_annot = cell_melt,
            grouping_vars = c("Locus", "Cell_type", "Assay", annot_cols),
            snp_filter = snp_filter
        )
        dat_melt$Cell_type <- cell_dict[dat_melt$Cell_type]
        if (sum(dat_melt$Count == 0 | is.na(dat_melt$Count), na.rm = T) > 0) {
            try({
                dat_melt[dat_melt$Count == 0 | is.na(dat_melt$Count), "Count"] <- NA
            })
        }
        dat_melt <- subset(dat_melt, !is.na(Count))
        dat_melt$background <- NA

        # Make sure locus order kept
        locus_order <- get_CS_counts(merged_DT)
        dat_melt$Locus <- factor(dat_melt$Locus,
            levels = locus_order$Locus,
            ordered = TRUE
        )
        return(dat_melt)
    } else {
        cell_melt$Cell_type <- cell_dict[cell_melt$Cell_type]
        gr.melt <- GenomicRanges::makeGRangesFromDataFrame(
            cell_melt,
            keep.extra.columns = TRUE,
            seqnames.field = "seqnames",
            start.field = "start",
            end.field = "end"
        )
        return(gr.melt)
    }
}
