#' Bar plot of tool-specific CS sizes
#'
#' Loci ordered by UCS size (smallest to largest).
#' 
#' @param show_numbers Print numbers on top of bars.
#' @param ylabel y-axis label.
#' @param label_yaxis Whether or not to label the y-axis.
#' @param legend_nrow Number of rows for the legend to span over.
#' @inheritParams CS_bin_plot
#' @inheritParams echodata::merge_finemapping_results
#' 
#' @family summarise
#' @export
#' @importFrom dplyr mutate arrange
#' @importFrom data.table melt.data.table data.table
#' @importFrom methods show
#' @importFrom echodata get_CS_counts
#' 
#' @examples
#' dat <- echodata::BST1
#' dat$Locus <- "BST1"
#' gg_CS <- echoannot::CS_counts_plot(merged_DT = dat)
CS_counts_plot <- function(merged_DT,
                           show_numbers = TRUE,
                           ylabel = "Locus",
                           legend_nrow = 3,
                           label_yaxis = TRUE,
                           top_CS_only = FALSE,
                           show_plot = TRUE) {
    
    requireNamespace("ggplot2")
    Locus <- UCS.CS_size <- Method <- `Credible Set size` <-
        CS <- Locus_UCS <- NULL
    locus_order <- echodata::get_CS_counts(merged_DT,
        top_CS_only = top_CS_only
    )
    melt.dat <-
        locus_order |>
        dplyr::mutate(Locus_UCS = paste0(Locus, "  (", UCS.CS_size, ")")) |>
        data.table::data.table() |>
        data.table::melt.data.table(
            measure.vars = grep(".CS_size$",
                colnames(locus_order),
                value = TRUE
            ),
            variable.name = "CS",
            value.name = "Credible Set size"
        ) |>
        dplyr::mutate(Method = gsub(".CS_size$", "", CS)) |>
        dplyr::arrange(Locus, Method) |>
        dplyr::mutate(Method = factor(Method)) |>
        subset(Method != "mean")
    melt.dat <- order_loci(dat = melt.dat, merged_DT = merged_DT)
    melt.dat[melt.dat$`Credible Set size` == 0 |
        is.na(melt.dat$`Credible Set size`), "Credible Set size"] <- NA


    ggplot2::ggplot(
        data = melt.dat,
        ggplot2::aes(y = Locus, x = `Credible Set size`, fill = Method)
    ) +
        ggplot2::geom_bar(stat = "identity", color = "white", size = .05) +
        ggplot2::geom_text(ggplot2::aes(label = `Credible Set size`),
            color = "grey20",
            size = 3, show.legend = FALSE,
            position = ggplot2::position_stack(vjust = .5)
        ) +
        ggplot2::geom_text(
            ggplot2::aes(x = sum(`Credible Set size`), label = Locus_UCS),
            size = 3, show.legend = FALSE,
            position = ggplot2::position_stack(vjust = 1)
        )

    ## Method-specific CS
    gg_CS <- ggplot2::ggplot(
        data = melt.dat,
        ggplot2::aes(y = Locus, x = `Credible Set size`, fill = Method)
    ) +
        ggplot2::geom_bar(stat = "identity", color = "white", size = .05) +
        ggplot2::labs(x = NULL, y = ylabel) +
        ggplot2::theme_bw() +
        ggplot2::theme(
            legend.position = "top", 
            panel.grid.major.x = ggplot2::element_blank(),
            panel.grid.minor.x = ggplot2::element_blank(),
            legend.text = ggplot2::element_text(size = 8),
            legend.key.size = ggplot2::unit(.5, units = "cm")
        ) +
        ggplot2::guides(fill = ggplot2::guide_legend(
            nrow = legend_nrow,
            title.position = "top",
            title.hjust = .5
        ))
    if (show_numbers) {
        gg_CS <- gg_CS +
            ggplot2::geom_text(ggplot2::aes(label = `Credible Set size`),
                color = "grey20",
                size = 3, show.legend = FALSE,
                position = ggplot2::position_stack(vjust = .5)
            ) +
            ggplot2::geom_text(
                ggplot2::aes(x = sum(`Credible Set size`),
                             label = Locus_UCS),
                size = 3, show.legend = FALSE,
                position = ggplot2::position_stack(vjust = 1)
            )
    }

    if (label_yaxis == FALSE) {
        gg_CS <- gg_CS + ggplot2::theme(axis.text.y = ggplot2::element_blank())
    }
    if (show_plot) suppressWarnings(methods::show(gg_CS))
    return(list(
        plot = gg_CS,
        data = melt.dat
    ))
}
