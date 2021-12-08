#' Get overlap between data table of SNPs and HiChIP_FitHiChIP
#' coaccessibility anchors
#'
#' Anchors are the genomic regions that have evidence of being
#' functionally connected to one another (coaccessible),
#'  e.g. enhancer-promoter interactions.
#'
#' @param query_dat Fine-mapping results.
#' @param verbose Print messages.
#'
#' @family CORCES2020
#' @importFrom GenomicRanges mcols
#' @source \url{https://doi.org/10.1038/s41588-020-00721-x}
#' @export 
CORCES2020_get_hichip_fithichip_overlap <- function(query_dat,
                                                    verbose = TRUE) {
    loops <- get_CORCES2020_hichip_fithichip_loop_calls()
    #### Anchor 1 ####
    gr.anchor1 <- echotabix::liftover(
        sumstats_dt = loops,
        ref_genome = "hg38",
        convert_ref_genome = "hg19",
        chrom_col = "hg38_Chromosome_Anchor1",
        start_col = "hg38_Start_Anchor1",
        end_col = "hg38_Stop_Anchor1",
        verbose = FALSE,
        as_granges = TRUE,
        style = "NCBI"
    )
    gr.anchor1_hits <- granges_overlap(
        dat1 = query_dat,
        chrom_col.1 = "CHR",
        start_col.1 = "POS",
        end_col.1 = "POS",
        dat2 = gr.anchor1
    )
    GenomicRanges::mcols(gr.anchor1_hits)["Anchor"] <- 1

    #### Anchor 2 ####
    gr.anchor2 <- echotabix::liftover(
        sumstats_dt = loops,
        ref_genome = "hg38",
        convert_ref_genome = "hg19",
        chrom_col = "hg38_Chromosome_Anchor2",
        start_col = "hg38_Start_Anchor2",
        end_col = "hg38_Stop_Anchor2",
        verbose = FALSE,
        as_granges = TRUE,
        style = "NCBI"
    )
    gr.anchor2_hits <- granges_overlap(
        dat1 = query_dat,
        chrom_col.1 = "CHR",
        start_col.1 = "POS",
        dat2 = gr.anchor2
    )
    GenomicRanges::mcols(gr.anchor2_hits)["Anchor"] <- 2
    #### Merge and report ####
    gr.anchor <- rbind_granges(gr.anchor1_hits, gr.anchor2_hits)
    GenomicRanges::mcols(gr.anchor)["Assay"] <- "HiChIP_FitHiChIP"
    # Have to make a pseudo cell-type col bc (i think)
    # this analysis was done on bulk data
    GenomicRanges::mcols(gr.anchor)["brain"] <- 1
    messager("+ CORCES2020:: Found", 
             formatC(length(gr.anchor), big.mark = ","),
        "hits with HiChIP_FitHiChIP coaccessibility loop anchors.",
        v = verbose
    )
    return(gr.anchor)
}

CORCES2020_get_HiChIP_FitHiChIP_overlap <- function(...){
    .Deprecated("CORCES2020_get_hichip_fithichip_overlap")
    CORCES2020_get_hichip_fithichip_overlap(...)
}
