#' Get overlap between data table of SNPs and HiChIP_FitHiChIP
#' coaccessibility anchors
#'
#' Anchors are the genomic regions that have evidence of being
#' functionally connected to one another (coaccessible),
#'  e.g. enhancer-promoter interactions.
#'
#' @param finemap_dat Fine-mapping results.
#' @param verbose Print messages.
#'
#' @family CORCES_2020
#' @source \url{https://doi.org/10.1038/s41588-020-00721-x}
#' @export
#'
CORCES_2020.get_HiChIP_FitHiChIP_overlap <- function(finemap_dat,
                                                     verbose = TRUE) {
    loops <- echoannot::CORCES_2020.HiChIP_FitHiChIP_loop_calls
    # Anchor 1
    gr.anchor1 <- echotabix::liftover(
        sumstats_dt = loops,
        convert_ref_genome = "hg19",
        chrom_col = "hg38_Chromosome_Anchor1",
        start_col = "hg38_Start_Anchor1",
        end_col = "hg38_Stop_Anchor1",
        verbose = FALSE,
        as_granges = TRUE,
        style = "NCBI"
    )
    gr.anchor1_hits <- granges_overlap(
        dat1 = finemap_dat,
        chrom_col.1 = "CHR",
        start_col.1 = "POS",
        end_col.1 = "POS",
        dat2 = gr.anchor1
    )
    gr.anchor1_hits$Anchor <- 1

    # Anchor 2
    gr.anchor2 <- echotabix::liftover(
        sumstats_dt = loops,
        convert_ref_genome = "hg19",
        chrom_col = "hg38_Chromosome_Anchor2",
        start_col = "hg38_Start_Anchor2",
        end_col = "hg38_Stop_Anchor2",
        verbose = FALSE,
        as_granges = TRUE,
        style = "NCBI"
    )
    gr.anchor2_hits <- granges_overlap(
        dat1 = finemap_dat,
        chrom_col.1 = "CHR",
        start_col.1 = "POS",
        end_col.1 = "POS",
        dat2 = gr.anchor2
    )
    gr.anchor2_hits$Anchor <- 2
    # Merge and report
    gr.anchor <- rbind_granges(gr.anchor1_hits, gr.anchor2_hits)
    gr.anchor$Assay <- "HiChIP_FitHiChIP"
    # Have to make a pseudo cell-type col bc (i think)
    # this analysis was done on bulk data
    gr.anchor$brain <- 1
    messager("+ CORCES_2020:: Found", length(gr.anchor),
        "hits with HiChIP_FitHiChIP coaccessibility loop anchors.",
        v = verbose
    )
    return(gr.anchor)
}
