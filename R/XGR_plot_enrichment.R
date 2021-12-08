#' Plot XGR enrichment
#' 
#' Plot XGR enrichment.
#' @family XGR
#' @keywords internal
#' @examples
#' \dontrun{
#' enrich_res <- XGR_iterate_enrichment(
#'     dat = echodata::get_Nalls2019_merged(),
#'     foreground_filter = "Consensus_SNP",
#'     background_filter = "leadSNP",
#'     lib.selections = c("ENCODE_TFBS_ClusteredV3_CellTypes")
#' )
#' XGR_plot_enrichment(enrich_res)
#' }
XGR_plot_enrichment <- function(enrich_res,
                                adjp_thresh = 0.05, 
                                top_annotations = NULL,
                                show_plot = TRUE) {
    
    requireNamespace("ggplot2")
    fc <- adjp <- assay <- NULL;
    enrich_res <- dplyr::arrange(enrich_res, dplyr::desc(fc))
    enrich_res$source <- factor(enrich_res$source,
        unique(enrich_res$source),
        ordered = TRUE
    )
    enrich_res$assay <- factor(enrich_res$assay,
        unique(enrich_res$assay),
        ordered = TRUE
    )
    if (is.null(top_annotations)) {
        top_annotations <- nrow(enrich_res)
    }

    gp <- ggplot2::ggplot(
        data = subset(enrich_res, adjp < adjp_thresh)[
            seq(1, top_annotations),
        ],
        ggplot2::aes(y = fc, x = assay, fill = fc)
    ) +
        ggplot2::geom_col() +
        ggplot2::labs(
            title = paste0(
                "Epigenomic annotation enrichment (FDR < ",
                adjp_thresh, ")"
            ),
            subtitle =
                "Foreground = Consensus SNPs\nBackground = Lead GWAS SNPs",
            y = "Fold-change"
        ) +
        ggplot2::facet_grid(
            facets = Cell_type ~ Assay,
            scales = "free",
            space = "free"
        ) +
        ggplot2::theme_bw() +
        ggplot2::theme(
            strip.placement = "outside",
            strip.text.y.left = ggplot2::element_text(angle = 0),
            strip.background = ggplot2::element_rect(color = "black", 
                                                     fill = "white"),
            plot.title = ggplot2::element_text(hjust = .5),
            plot.subtitle = ggplot2::element_text(hjust = .5)
        )
    if (show_plot) {
        print(gp)
    }
    return(gp)
}
