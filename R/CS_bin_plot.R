#' Plot CS bin counts
#'
#' @family summarise
#' @examples
#' bin_plot <- CS_bin_plot(merged_DT = echodata::Nalls2019_merged)
#' @export
#' @import ggplot2
#' @importFrom RColorBrewer brewer.pal
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
    custom_colors_dict <- setNames(
        custom_colors[seq(1, length(used_bins))],
        rev(used_bins)
    )
    custom_colors_dict[names(custom_colors_dict) == "0"] <- "lightgray"

    bin_plot <- ggplot(
        subset(bin_counts, Method != "mean"),
        aes(x = Method, fill = bin)
    ) +
        geom_bar(
            stat = "count", show.legend = TRUE,
            position = position_stack(reverse = FALSE), color = "white"
        ) +
        # scale_fill_brewer(palette = "Spectral", direction = -1) +
        scale_fill_manual(values = custom_colors_dict) +
        # geom_text(aes(label = paste(bin,"SNPs")),
        # position =  position_stack(vjust = .5), vjust=-1, stat = "count") +
        geom_text(aes(label = ..count..),
            position = position_stack(vjust = .5),
            vjust = .5, stat = "count"
        ) +
        theme_bw() +
        labs(x = NULL, y = "Loci", fill = "CS size") +
        coord_flip() +
        theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            rect = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks = element_blank(),
            legend.position = "top"
        ) +
        guides(fill = guide_legend(nrow = 1, reverse = TRUE))
    if (show_plot) print(bin_plot)
    return(list(
        plot = bin_plot,
        data = bin_counts
    ))
}
