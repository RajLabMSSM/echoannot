#' Plot overlap between some SNP group and various epigenomic data
#'
#' @param include.NOTT_2019_peaks Plot SNP subset overlap with
#'  peaks from cell-type-specific bulk ATAC, H3K27ac, and H3K4me3 assays.
#' @param include.NOTT_2019_enhancers_promoters Plot SNP subset overlap with
#' cell enhancers and promoters.
#' @param include.CORCES_2020_scATACpeaks Plot SNP subset overlap with
#' cell-type-specific scATAC-seq peaks.
#' @param include.CORCES_2020_Cicero_coaccess Plot SNP subset overlap with
#' Cicero coaccessibility peaks (derived from scATACseq).
#' @keywords internal
#' @family summarise
#' @source
#' \href{https://science.sciencemag.org/content/366/6469/1134}{
#' Nott et al. (2019)}
#' \href{https://www.biorxiv.org/content/10.1101/2020.01.06.896159v1}{
#' Corces et al. (2020/bioRxiv)}
#' @examples
#' # Consensus SNPs
#' gg_peaks <- peak_overlap_plot(
#'     merged_DT = echodata::Nalls2019_merged,
#'     snp_filter = "Consensus_SNP==T",
#'     fill_title = "Consensus SNPs in epigenomic peaks"
#' )
#' # UCS SNPs
#' gg_peaks <- peak_overlap_plot(
#'     merged_DT = echodata::Nalls2019_merged,
#'     snp_filter = "Support>0",
#'     fill_title = "UCS SNPs in epigenomic peaks"
#' )
#' @export
#' @import ggplot2
#' @importFrom patchwork plot_layout
#' @importFrom scales alpha
#' @importFrom stats formula
peak_overlap_plot <- function(merged_DT,
                              snp_filter = "Consensus_SNP==TRUE",
                              include.NOTT_2019_peaks = TRUE,
                              include.NOTT_2019_enhancers_promoters = TRUE,
                              include.NOTT_2019_PLACseq = TRUE,
                              include.CORCES_2020_scATACpeaks = TRUE,
                              include.CORCES_2020_Cicero_coaccess = TRUE,
                              include.CORCES_2020_bulkATACpeaks = TRUE,
                              include.CORCES_2020_HiChIP_FitHiChIP_coaccess = TRUE,
                              include.CORCES_2020_gene_annotations = TRUE,
                              plot_celltype_specificity = TRUE,
                              plot_celltype_specificity_genes = FALSE,
                              facets_formula = ". ~ Cell_type",
                              show_plot = TRUE,
                              label_yaxis = TRUE,
                              x_strip_angle = 90,
                              x_tick_angle = 40,
                              drop_empty_cols = F,
                              fill_title = paste(snp_filter, "\nin epigenomic peaks"),
                              save_path = F,
                              height = 11,
                              width = 12,
                              subplot_widths = c(1, .5),
                              verbose = TRUE) {
    # verbose=T;include.NOTT_2019_peaks=T; include.NOTT_2019_enhancers_promoters=T; include.NOTT_2019_PLACseq=T; include.CORCES_2020_scATACpeaks=T;
    # include.CORCES_2020_Cicero_coaccess=T;include.CORCES_2020_bulkATACpeaks=T;include.CORCES_2020_HiChIP_FitHiChIP_coaccess=T;include.CORCES_2020_gene_annotations=T;
    # no_no_loci<- c("HLA-DRB5","MAPT","ATG14","SP1","LMNB1","ATP6V0A1",
    #                "RETREG3","UBTF","FAM171A2","MAP3K14","CRHR1","MAPT-AS1","KANSL1","NSF","WNT3")
    # merged_DT <- subset(merged_DT, !Locus %in% no_no_loci)
    # x_strip_angle=90;  facets_formula=". ~ Cell_type";  drop_empty_cols=F; x_tick_angle=40;  snp_filter="Consensus_SNP==T";
    # fill_title=paste(snp_filter,"\nin epigenomic peaks"); subplot_widths = c(1,.5);plot_celltype_specificity_genes=F;

    Locus <- Assay <- Count <- background <- NULL

    dat_melt <- data.frame()
    ######## NOTT et al. 2019 #########
    if (include.NOTT_2019_peaks) {
        try({
            dat_melt.NOTTpeaks <- NOTT_2019.prepare_peak_overlap(
                merged_DT = merged_DT,
                snp_filter = snp_filter
            )
            dat_melt.NOTTpeaks$background <- NA
            dat_melt.NOTTpeaks$Study <- "Nott et al. (2019)"
            dat_melt <- base::rbind(dat_melt, dat_melt.NOTTpeaks)
        })
    }

    if (include.NOTT_2019_enhancers_promoters) {
        try({
            dat_melt.NOTTreg <- NOTT_2019.prepare_regulatory_overlap(
                merged_DT = merged_DT,
                snp_filter = snp_filter
            )
            dat_melt.NOTTreg$background <- 1
            dat_melt.NOTTreg$Study <- "Nott et al. (2019)"
            dat_melt <- base::rbind(dat_melt, dat_melt.NOTTreg)
        })
    }

    if (include.NOTT_2019_PLACseq) {
        try({
            dat_melt.NOTTplac <- NOTT_2019.prepare_placseq_overlap(
                merged_DT = merged_DT,
                snp_filter = snp_filter
            )
            dat_melt.NOTTplac$background <- NA
            dat_melt.NOTTplac$Study <- "Nott et al. (2019)"
            dat_melt <- base::rbind(dat_melt, dat_melt.NOTTplac, fill = TRUE)
        })
    }

    ######## CORCES et al. 2020 #########
    if (include.CORCES_2020_scATACpeaks) {
        try({
            dat_melt.CORCES_scPeaks <- CORCES_2020.prepare_scATAC_peak_overlap(
                merged_DT = merged_DT,
                snp_filter = snp_filter,
                add_cicero = include.CORCES_2020_Cicero_coaccess,
                annotate_genes = include.CORCES_2020_gene_annotations,
                verbose = verbose
            )
            dat_melt.CORCES_scPeaks$background <- NA
            dat_melt.CORCES_scPeaks$Study <- "Corces et al. (2020)"
            dat_melt <- base::rbind(dat_melt, dat_melt.CORCES_scPeaks, fill = TRUE)
        })
    }
    if (include.CORCES_2020_bulkATACpeaks) {
        try({
            dat_melt.CORCES_bulkPeaks <- CORCES_2020.prepare_bulkATAC_peak_overlap(
                merged_DT = merged_DT,
                snp_filter = snp_filter,
                add_HiChIP_FitHiChIP = include.CORCES_2020_HiChIP_FitHiChIP_coaccess,
                annotate_genes = include.CORCES_2020_gene_annotations,
                verbose = verbose
            )
            dat_melt.CORCES_bulkPeaks$background <- NA
            dat_melt.CORCES_bulkPeaks$Study <- "Corces et al. (2020)"
            dat_melt <- base::rbind(dat_melt,
                dat_melt.CORCES_bulkPeaks,
                fill = TRUE
            )
        })
    }
    ## Account for situations where
    ## include.CORCES_2020_bulkATACpeaks=F or no overlap was found
    if (!"Gene_Symbol" %in% colnames(dat_melt)) dat_melt$Gene_Symbol <- NA
    if (!"Annotation" %in% colnames(dat_melt)) dat_melt$Annotation <- NA



    plot_dat <- order_loci(
        dat = dat_melt,
        merged_DT = merged_DT
    )
    plot_dat$Assay <- factor(plot_dat$Assay,
        levels = c(
            "H3K27ac", "H3K4me3",
            "ATAC", "bulkATAC", "scATAC", "PLAC",
            "Cicero", "HiChIP_FitHiChIP",
            "enhancers", "promoters"
        ),
        ordered = TRUE
    )
    if (x_strip_angle != 90) {
        plot_dat$Cell_type <- gsub(" ", "\n", plot_dat$Cell_type)
    }
    neuronal_cols <- grep("neuron", unique(plot_dat$Cell_type),
        value = TRUE
    )
    plot_dat$Cell_type <- factor(plot_dat$Cell_type,
        levels = c(
            "astrocytes", "microglia",
            "oligo", "OPCs",
            neuronal_cols, "brain"
        ),
        ordered = TRUE
    )
    plot_dat$background <- as.numeric(plot_dat$background)
    ### Double check there's no errors
    plot_dat <- subset(plot_dat, !is.na(Locus))
    ### Make sure there's no missing loci
    # unique(plot_dat$Locus)==unique(merged_DT$Locus)

    # Plot
    gg_pks <- ggplot(data = plot_dat, aes(x = Assay, y = Locus, fill = Count)) +
        geom_tile(color = "white") +
        # scale_fill_manual(values = consensus_colors) +
        # scale_fill_discrete(na.value = "transparent") +
        scale_fill_gradient(
            na.value = "transparent",
            low = scales::alpha("blue", .7),
            high = scales::alpha("red", .7)
        ) +
        # geom_point(aes(size=ifelse(Count>0, "dot", "no_dot")),
        # show.legend = F, alpha=.8, color="white") +

        # geom_rect( aes(xmin = Assay, xmax = dplyr::lead(Assay),
        # ymin = -0.5, ymax = Inf, fill = background),
        #           alpha = 0.5, color="grey") +
        geom_tile(
            data = subset(plot_dat, !is.na(background)),
            aes(width = 0.9, height = 0.9), color = "cyan", size = .7
        ) +
        facet_grid(
            facets = stats::formula(facets_formula),
            scales = if (drop_empty_cols) "free_x" else "fixed",
            space = "free_x"
        ) +
        scale_size_manual(values = c(dot = .5, no_dot = NA), guide = "none") +
        labs(fill = fill_title) +
        theme_bw() +
        theme(
            legend.position = "top",
            legend.title.align = .5,
            axis.text.x = element_text(
                angle = x_tick_angle,
                hjust = if (x_tick_angle > 0) 1 else NULL
            ),
            # legend.background =  element_rect(fill = "lightgray"),
            legend.key = element_rect(colour = "gray60"),
            axis.title.y = element_blank(),
            strip.background = element_rect(fill = "grey90"),
            strip.text.x = element_text(angle = x_strip_angle),
            legend.text = element_text(size = 8),
            legend.text.align = .5,
            # legend.key.size = unit(.5, units = "cm" ),
            legend.box = "horizontal",
            panel.background = element_rect(fill = "transparent"),
            # panel.grid = element_line(color="gray", size=5),
            # panel.grid.major.x = element_line(color="gray", size=.5),
            # panel.grid.major.x = element_line(color="gray", size=.5),

            panel.grid.minor = element_line(color = "white", size = .5),
            plot.margin = unit(rep(.1, 4), "cm")
        ) +
        guides(color = guide_legend(
            nrow = 1, reverse = F,
            title.position = "top",
            # label.position = "top",
            title.hjust = .5,
            label.hjust = -1
        )) +
        # Keep unused levels/Loci
        scale_y_discrete(drop = FALSE)
    if (label_yaxis == F) {
        gg_pks <- gg_pks + theme(axis.text.y = element_blank())
    }

    if (plot_celltype_specificity) {
        try({
            gg_cells <- cell_type_specificity(
                plot_dat = plot_dat,
                merged_DT = merged_DT,
                label_yaxis = F,
                show_genes = plot_celltype_specificity_genes,
                y_lab = NULL,
                x_strip_angle = x_strip_angle,
                show_plot = FALSE
            )
            gg_pks <- gg_pks + gg_cells$plot +
                patchwork::plot_layout(nrow = 1, widths = subplot_widths)
        })
    }

    if (show_plot) print(gg_pks)
    if (save_path != FALSE) {
        messager("+ Saving plot ==>", save_path, v = verbose)
        ggplot2::ggsave(save_path, gg_pks, height = height, width = width)
    }
    return(list(
        data = dat_melt,
        plot = gg_pks
    ))
}