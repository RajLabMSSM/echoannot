#' Get cell-type-specifity score for each cell type
#'
#' Aggregate SNP overlap across various epigenomic datasets
#' and then identify the number of SNPs overlapping by each cell type
#' 
#' @param plot_specificity Plot cell-type specificity scores 
#' for locus instead of raw assay overlap counts. 
#' @keywords internal
#' @importFrom dplyr top_n group_by slice_max
#' @importFrom methods show
#' @importFrom data.table data.table dcast merge.data.table
cell_type_specificity <- function(plot_dat,
                                  merged_DT,
                                  plot_specificity = TRUE,
                                  min_count = NULL,
                                  top_celltype_only = FALSE,
                                  label_yaxis = TRUE,
                                  y_lab = NULL,
                                  show_genes = FALSE,
                                  x_strip_angle = 40,
                                  show_plot = TRUE) {
    
    requireNamespace("ggplot2")
    Count <- Locus <- Cell_group <- Assay_count <- Gene_Symbol <-
        Annotation <- Cell_type <- SNP <- NULL
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

    cell_tally <- data.table::data.table(plot_dat) |>
        unique() |>
        dplyr::mutate(
            Assay_count = ifelse(Count > 0, 1, 0), # Set any overlap ==1
            Cell_group = factor(Cell_group_dict[Cell_type],
                levels = unique(unname(Cell_group_dict)),
                ordered = TRUE
            )
        ) |>
        dplyr::group_by(Locus, Cell_group, .drop = FALSE) |>
        dplyr::summarise(
            Assay_count = sum(Assay_count, na.rm = TRUE),
            SNP_Count = sum(Count, na.rm = TRUE),
            SNP = paste(unique(SNP), collapse = ";"),
            Gene_Symbol = paste(unique(as.character(
                stats::na.omit(gsub("^NA$", NA, Gene_Symbol))
                )), collapse = ";"),
            Annotation = paste(unique(as.character(
                stats::na.omit(Annotation)
                )), collapse = ";")
        ) |>
        unique() |>
        data.table::data.table()
    #### Compute specificity score ####
    counts <- data.table::dcast(cell_tally, 
                                     formula = Locus ~ Cell_group, 
                                     value.var = "Assay_count", 
                                     drop = FALSE)     
    counts <- as.matrix(counts[,-1]) |> `rownames<-`(counts$Locus) 
    specificity <- data.table::data.table(
        counts/ rowSums(counts, na.rm = TRUE), 
        keep.rownames = "Locus") 
    if(plot_specificity){
        cell_tally <- data.table::melt.data.table(
            data = specificity, 
            id.vars = "Locus", 
            variable.name = "Cell_group", 
            value.name = "Specificity") |>
            data.table::merge.data.table(y = cell_tally)
    }
    #### Only choose top cell-type per locus ####
    if (top_celltype_only) {
        cell_tally <- cell_tally |>
            dplyr::group_by(Locus) |>
            dplyr::slice_max(n = 1, order_by = Assay_count)
    }
    if (!is.null(min_count)) {
        cell_tally[cell_tally$Count < min_count, "Count"] <- NA
    }

    cell_tally <- order_loci(
        dat = cell_tally,
        merged_DT = merged_DT
    )
    gg_tally <- ggplot2::ggplot(
        data = cell_tally,
        ggplot2::aes_string(x = "Cell_group",
                            y = "Locus", 
                            fill = if(plot_specificity){
                                "Specificity"
                            }else{"Assay_count"})
    ) +
        ggplot2::geom_tile(color = "white") +
        ggplot2::facet_grid(
            facets = . ~ Cell_group,
            scales = "free_x"
        ) +
        ggplot2::scale_fill_viridis_c(
            na.value = "transparent") +
        ggplot2::labs(y = y_lab) + 
        ggplot2::theme_bw() + 
        ggplot2::theme(
            axis.text.x = ggplot2::element_blank(), 
            legend.box = "horizontal",
            legend.position = "top",
            legend.text = ggplot2::element_text(size = 8),
            legend.text.align = .5,
            strip.text.x = ggplot2::element_text(angle = x_strip_angle,
                                                 color = "white"),
            strip.background.x = ggplot2::element_rect(fill = "black"),
            panel.spacing = ggplot2::unit(.1, "lines"),
            plot.margin = ggplot2::unit(c(.1, 2, .1, .1), "cm")
        ) +
        ggplot2::scale_y_discrete(drop = FALSE)
    if (show_genes) {
        gg_tally <- gg_tally +
            ggplot2::geom_text(
                ggplot2::aes(label = eval(parse(text = "Gene_Symbol"))),
                size = 3, color = "cyan"
            )
    }
    if (isFALSE(label_yaxis)) {
        gg_tally <- gg_tally + 
            ggplot2::theme(axis.text.y = ggplot2::element_blank())
    }
    if (label_yaxis == "right") {
        gg_tally <- gg_tally + 
            ggplot2::scale_y_discrete(position = "right")
    }
    if (show_plot) methods::show(gg_tally)
    return(list(
        plot = gg_tally,
        data = cell_tally,
        specificity = specificity
    ))
}
