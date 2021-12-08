#' Plot CS bin counts
#'
#' @family summarise
#' @examples
#' bin_plot <- CS_bin_plot(merged_DT = echodata::get_Nalls2019_merged())
#' @export
#' @importFrom RColorBrewer brewer.pal
#' @importFrom stats setNames
CS_bin_plot <- function(merged_DT,
                        show_plot = TRUE) {
    Method <- bin <- ..count.. <- NULL

    bin_counts <- get_CS_bins(merged_DT = merged_DT)
    # Assign bin colors
    used_bins <- levels(bin_counts$bin)[levels(bin_counts$bin) %in%
        unique(bin_counts$bin)]
    custom_colors <- RColorBrewer::brewer.pal(
        n = length(levels(bin_counts$bin)), "GnBu"
    )
    custom_colors_dict <- stats::setNames(
        custom_colors[seq(1, length(used_bins))],
        rev(used_bins)
    )
    custom_colors_dict[names(custom_colors_dict) == "0"] <- "lightgray"

    bin_plot <- ggplot2::ggplot(
        subset(bin_counts, Method != "mean"),
        ggplot2::aes(x = Method, fill = bin)
    ) +
        ggplot2::geom_bar(
            stat = "count", show.legend = TRUE,
            position = ggplot2::position_stack(reverse = FALSE), color = "white"
        ) +
        # scale_fill_brewer(palette = "Spectral", direction = -1) +
        ggplot2::scale_fill_manual(values = custom_colors_dict) +
        # geom_text(aes(label = paste(bin,"SNPs")),
        # position =  position_stack(vjust = .5), vjust=-1, stat = "count") +
        ggplot2::geom_text(ggplot2::aes(label = ..count..),
            position = ggplot2::position_stack(vjust = .5),
            vjust = .5, stat = "count"
        ) +
        ggplot2::theme_bw() +
        ggplot2::labs(x = NULL, y = "Loci", fill = "CS size") +
        ggplot2::coord_flip() +
        ggplot2::theme(
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            rect = ggplot2::element_blank(),
            axis.text.x = ggplot2::element_blank(),
            axis.ticks = ggplot2::element_blank(),
            legend.position = "top"
        ) +
        ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, reverse = TRUE))
    if (show_plot) print(bin_plot)
    return(list(
        plot = bin_plot,
        data = bin_counts
    ))
}
