#' Get cell-type-specifity score for each cell type
#'
#' Aggregate SNP overlap across various epigenomic datasets
#' and then identify the number of SNPs overlapping by each cell type
#'
#' @keywords internal
#' @rawNamespace import(ggplot2, except = c(geom_rect, ggsave))
#' @importFrom dplyr %>% top_n
cell_type_specificity <- function(plot_dat,
                                  merged_DT,
                                  min_count = NULL,
                                  top_celltype_only = FALSE,
                                  label_yaxis = TRUE,
                                  y_lab = NULL,
                                  show_genes = FALSE,
                                  x_strip_angle = 40,
                                  show_plot = TRUE) {
    Count <- Locus <- Cell_group <- Assay_count <- Gene_Symbol <- Annotation <-
        NULL
    Cell_group_dict <- c(
        "astrocytes" = "astrocytes",
        "microglia" = "microglia",
        "oligo" = "oligo",
        "OPCs" = "oligo",
        "neurons" = "neurons",
        "neurons (+)" = "neurons",
        "neurons (-)" = "neurons",
        "neurons (nigral)" = "neurons",
        "brain" = "brain"
    )

    cell_tally <- plot_dat %>%
        dplyr::mutate(
            Assay_count = ifelse(Count > 0, 1, 0), # Set any overlap ==1
            Cell_group = factor(Cell_group_dict[Cell_type],
                levels = unique(unname(Cell_group_dict)),
                ordered = TRUE
            )
        ) %>%
        dplyr::group_by(Locus, Cell_group, .drop = F) %>%
        dplyr::summarise(
            Assay_count = sum(Assay_count, na.rm = TRUE),
            SNP_Count = sum(Count, na.rm = TRUE),
            Gene_Symbol = gsub("^NA$", NA, Gene_Symbol),
            Annotation = Annotation
        )

    if (top_celltype_only) {
        cell_tally <- dplyr::top_n(cell_tally, n = 1, wt = "Cell_group")
    }
    if (!is.null(min_count)) {
        cell_tally[cell_tally$Count < min_count, "Count"] <- NA
    }

    cell_tally <- order_loci(
        dat = cell_tally,
        merged_DT = merged_DT
    )
    gg_tally <- ggplot(
        data = cell_tally,
        aes(x = Cell_group, y = Locus, fill = Assay_count)
    ) +
        geom_tile(color = "white") +
        facet_grid(
            facets = . ~ Cell_group,
            scales = "free_x"
        ) +
        scale_fill_viridis_c(na.value = "transparent") +
        labs(y = y_lab) +
        theme_bw() +
        # scale_x_discrete(position = "top") +
        theme(
            axis.text.x = element_blank(),
            # element_text(angle = x_text_angle, hjust = 0),
            legend.box = "horizontal",
            legend.position = "top",
            legend.text = element_text(size = 8),
            legend.text.align = .5,
            strip.text.x = element_text(angle = x_strip_angle, color = "white"),
            strip.background.x = element_rect(fill = "black"),
            panel.spacing = unit(.1, "lines"),
            plot.margin = unit(c(.1, 2, .1, .1), "cm")
        ) +
        scale_y_discrete(drop = FALSE)
    if (show_genes) {
        gg_tally <- gg_tally +
            geom_text(aes(label = eval(parse(text = "Gene_Symbol"))),
                size = 3, color = "cyan"
            )
    }
    if (label_yaxis == F) {
        gg_tally <- gg_tally + theme(axis.text.y = element_blank())
    }
    if (label_yaxis == "right") {
        gg_tally <- gg_tally + scale_y_discrete(position = "right")
    }
    if (show_plot) print(gg_tally)
    return(list(
        plot = gg_tally,
        data = cell_tally
    ))
}
