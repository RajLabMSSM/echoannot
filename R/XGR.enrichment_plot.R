#' Plot enrichment results
#'
#' @family XGR
#' @examples
#' \dontrun{
#' root <- file.path(
#'     "/sc/arion/projects/pd-omics/brian",
#'     "Fine_Mapping/Data/GWAS/Nalls23andMe_2019/_genome_wide"
#' )
#' ### merged enrichment results
#' enrich_res <- data.table::fread(
#'     file.path(
#'         root,
#'         "XGR/celltypespecific_epigenomics.SNP_groups.csv.gz"
#'     )
#' )
#' enrich_res <- data.table::fread(
#'     file.path(
#'         root,
#'         "XGR/celltypespecific_epigenomics.snp_groups.csv.gz"
#'     )
#' )
#' enrich_boot <- data.table::fread(
#'     file.path(
#'         root,
#'         "XGR/celltypespecific_epigenomics.snp_groups.permute.csv.gz"
#'     )
#' )
#' enrich_assay <- data.table::fread(
#'     file.path(
#'         root,
#'         "XGR/celltypespecific_epigenomics.snp_groups.assay.csv.gz"
#'     )
#' )
#'
#' # Merged volcano plot
#' enrich_res <- subset(enrich_res, SNP_Group != "Consensus (-PolyFun)") %>%
#'     dplyr::rename(SNP_group = SNP_Group)
#' gp <- XGR.enrichment_plot(
#'     enrich_res = subset(enrich_res, !Assay %in% c("HiChIP_FitHiChIP", "PLAC")),
#'     title = "Enrichment: Cell-type-specific epigenomics",
#'     plot_type = "point",
#'     save_plot = file.path(
#'         root, "XGR/celltypespecific_epigenomics.enrich_volcano.png"
#'     ),
#'     height = 6, width = 8, shape_var = "Assay"
#' )
#' ## Merged bar plot
#' gp <- XGR.enrichment_plot(
#'     enrich_res = enrich_res,
#'     plot_type = "bar",
#'     facet_formula = ".~Assay",
#'     FDR_thresh = .05
#' )
#' # Merged volcano plot (permuted)
#' gp <- XGR.enrichment_plot(
#'     enrich_res = enrich.scATAC.permute,
#'     title = "Permuted enrichment: Cell-type-specific peaks and elements",
#'     plot_type = "point"
#' )
#' }
XGR.enrichment_plot <- function(enrich_res,
                                title = NULL,
                                subtitle = NULL,
                                facet_formula = NULL,
                                line_formula = "y ~ x",
                                line_method = "lm",
                                line_span = 1,
                                FDR_thresh = 1,
                                plot_type = "bar",
                                shape_var = "Cell_type",
                                facet_scales = "free",
                                show_plot = TRUE,
                                save_plot = FALSE,
                                height = 5,
                                width = 5) {
    enrich_res <- dplyr::mutate(
        enrich_res,
        SNP_group = factor(SNP_group,
            levels = unique(SNP_group),
            ordered = TRUE
        ),
        ## Make Random size smaller (otherwise will
        # make everything else relatively tiny)
        nOverlap = ifelse(SNP_group == "Random", 10, nOverlap)
    )
    sum(enrich_res$fc == -Inf)
    colorDict <- snp_group_colorDict()
    if (plot_type == "bar") {
        gp <- ggplot(
            data = subset(enrich_res, FDR <= FDR_thresh),
            aes(x = SNP_group, y = fc, fill = SNP_group)
        ) +
            # geom_col(stat="identity", alpha=.5, show.legend = F) +
            geom_boxplot() +
            geom_jitter(height = 0, width = 0, alpha = .1, show.legend = F) +
            scale_fill_manual(values = colorDict) +
            # ggpubr::stat_compare_means(method = method,
            #                            comparisons = comparisons,
            #                            label = "p.signif", size=3, vjust = 1.5) +
            facet_grid(
                facets = if (is.null(facet_formula)) {
                    facet_formula
                } else {
                    as.formula(facet_formula)
                },
                scales = "free_y"
            ) +
            labs(x = "SNP Group", title = title, subtitle = subtitle) +
            theme_bw() +
            theme(
                strip.background = element_rect(fill = "grey20"),
                strip.text = element_text(color = "white"),
                axis.text.x = element_text(angle = 45, hjust = 1)
            )
    }

    if (plot_type == "point") {
        gp <- ggplot(
            data = subset(enrich_res, FDR <= FDR_thresh),
            aes(
                x = log1p(fc), y = -log10(pvalue),
                size = nOverlap, color = SNP_group, group = SNP_group,
                fill = SNP_group,
                shape = eval(parse(text = shape_var))
            )
        ) +
            geom_smooth(
                alpha = 0.1, size = 0, span = line_span,
                method = line_method, formula = line_formula
            ) +
            stat_smooth(
                geom = "line", alpha = 0.3, size = 1, span = line_span,
                method = line_method, formula = line_formula
            ) +
            geom_point(alpha = .5) +
            scale_color_manual(values = colorDict) +
            scale_fill_manual(values = colorDict) +
            scale_shape_manual(
                values = seq(12, (12 + dplyr::n_distinct(
                    enrich_res[[shape_var]]
                )))
            ) +
            geom_hline(yintercept = -log10(0.05), linetype = 2, alpha = .5) +
            facet_grid(
                facets = if (is.null(facet_formula)) {
                    facet_formula
                } else {
                    as.formula(facet_formula)
                },
                scales = facet_scales
            ) +
            labs(title = title, subtitle = subtitle, shape = shape_var) +
            theme_bw() +
            theme(
                strip.background = element_rect(fill = "grey20"),
                strip.text = element_text(color = "white")
            )
    }

    if (show_plot) print(gp)

    if (save_plot != FALSE) {
        ggsave(save_plot, gp, dpi = 400, height = height, width = width)
    }
    return(gp)
}
