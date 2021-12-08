CORCES2020_scATAC_to_granges <- function(standardize_cellTypes = FALSE) {
    
    value <- NULL;
    scATAC <- data.table::melt.data.table(
        get_CORCES2020_scATACseq_celltype_peaks(),
        measure.vars = c(
            "ExcitatoryNeurons",
            "InhibitoryNeurons",
            "NigralNeurons", "Microglia",
            "Oligodendrocytes",
            "Astrocytes",
            "OPCs"
        ),
        variable.name = "Cell_type"
    ) %>% subset(value == 1)
    scATAC$Assay <- "scATAC"
    scATAC$Study <- "Corces2020.peaks"
    gr.Corces2020.peaks <- echotabix::liftover(
        dat = scATAC,
        ref_genome = "hg38",
        convert_ref_genome = "hg19",
        chrom_col = "hg38_Chromosome",
        start_col = "hg38_Start",
        end_col = "hg38_Stop",
        as_granges = TRUE,
        style = "NCBI"
    )
    if (standardize_cellTypes) {
        GenomicRanges::mcols(gr.Corces2020.peaks)["Cell_type"] <-
            standardize_celltypes(gr.Corces2020.peaks$Cell_type)
    }
    return(gr.Corces2020.peaks)
}