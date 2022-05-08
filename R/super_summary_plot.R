#' Merge all summary plots into one super plot
#'
#' @family summarise
#' @param merged_DT Merged fine-mapping results data from
#'  \link[echolocatoR]{finemap_loci}.
#' @param snp_filter Filter to use apply to SNPs before plotting.
#' @param coloc_results Colocalization results from 
#' \href{https://github.com/RajLabMSSM/catalogueR}{catalogueR}.
#' @param plot_missense Whether to include the missense mutations plot.
#' \emph{Warning:} Can take a lot time to query Biomart.
#' @param show_plot Show plot.
#' @param save_plot Save plot.
#' @inheritParams ggplot2::ggsave
#' 
#' @export
#' @importFrom patchwork plot_spacer plot_layout
#' @examples 
#' \dontrun{
#' merged_DT <- echodata::get_Nalls2019_merged() 
#' super_plot <- echoannot::super_summary_plot(merged_DT = merged_DT,
#'                                             plot_missense = FALSE)
#' }
super_summary_plot <- function(merged_DT,
                               snp_filter = "Consensus_SNP==TRUE",
                               coloc_results = NULL,
                               plot_missense = TRUE,
                               show_plot = TRUE,
                               save_plot = FALSE,
                               height = 15,
                               width = 13,
                               dpi = 300) {
    
    requireNamespace("ggplot2")
    bin_plot <- CS_bin_plot(
        merged_DT = merged_DT,
        show_plot = FALSE
    )

    if (!is.null(coloc_results)) {
        gg_egene <- coloc_nominated_egenes(
            coloc_results = coloc_results,
            merged_DT = merged_DT,
            credset_thresh = .8,
            fill_var = NULL,
            text_size = 2.5,
            y_lab = "Locus",
            x_lab = NULL,
            label_yaxis = TRUE,
            show_plot = FALSE
        )
        gg_egene_width <- .125
    } else {
        gg_egene <- c()
        gg_egene$plot <- patchwork::plot_spacer()
        gg_egene_width <- 0
    }

    gg_CS <- CS_counts_plot(
        merged_DT = merged_DT,
        show_numbers = FALSE,
        label_yaxis = FALSE,
        ylabel = NULL,
        show_plot = FALSE
    )
    #### plot_missense ####
    if (plot_missense) {
        try({
            gg_missense <- plot_missense(
                merged_DT = merged_DT,
                snp_filter = snp_filter,
                show.legend = FALSE,
                show_plot = FALSE
            )
        })
    }
    # In case biomart times out
    if (!exists("gg_missense")) {
        gg_missense <- c()
        gg_missense$plot <- patchwork::plot_spacer()
        gg_missense_width <- 0
    } else {
        gg_missense_width <- .01
    }

    gg_peaks <- peak_overlap_plot(
        merged_DT = merged_DT,
        snp_filter = snp_filter,
        include.NOTT2019_peaks = TRUE,
        include.NOTT2019_enhancers_promoters = TRUE,
        include.NOTT2019_PLACseq = TRUE,
        include.CORCES2020_scATACpeaks = TRUE,
        include.CORCES2020_Cicero_coaccess = FALSE,
        include.CORCES2020_bulkATACpeaks = TRUE,
        include.CORCES2020_HiChIP_FitHiChIP_coaccess = TRUE,
        include.CORCES2020_gene_annotations = TRUE,
        plot_celltype_specificity = TRUE,
        facets_formula = ". ~ Cell_type",
        show_plot = FALSE,
        label_yaxis = TRUE,
        subplot_widths = c(1, .2),
        x_strip_angle = 90,
        drop_empty_cols = TRUE,
        fill_title = paste(snp_filter, "SNPs\nin epigenomic peaks"),
        # save_path="~/Desktop/super_peak_plot.png",
        verbose = TRUE
    )
    # Merge
    gg_merged <- (patchwork::plot_spacer() +
        bin_plot$plot +
        patchwork::plot_layout(widths = c(.4, .6))) /
        (gg_egene$plot + gg_CS$plot +
            gg_missense$plot + gg_peaks$plot +
            patchwork::plot_layout(widths = c(
                gg_egene_width, .3,
                gg_missense_width, 1
            ))) +
        patchwork::plot_layout(heights = c(.15, 1), ncol = 1)

    if (show_plot) print(gg_merged)
    if (save_plot != FALSE) {
        ggplot2::ggsave(save_plot,
            gg_merged,
            dpi = dpi,
            height = height, 
            width = width
        )
    }
    return(list(
        data = gg_peaks$data,
        plot = gg_merged
    ))
}
