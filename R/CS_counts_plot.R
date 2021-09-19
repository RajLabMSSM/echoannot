#' Bar plot of tool-specific CS sizes
#'
#' Loci ordered by UCS size (smallest to largest).
#' @family summarise
#' @examples
#' gg_CS <- CS_counts_plot(merged_DT = echodata::get_Nalls2019_merged())
#' @export
#' @rawNamespace import(ggplot2, except = c(geom_rect, ggsave))
#' @importFrom dplyr %>% mutate arrange
CS_counts_plot <- function(merged_DT,
                           show_numbers = TRUE,
                           ylabel = "Locus",
                           legend_nrow = 3,
                           label_yaxis = TRUE,
                           top_CS_only = FALSE,
                           show_plot = TRUE) {
    Locus <- UCS.CS_size <- Method <- `Credible Set size` <- Locus_UCS <- NULL

    locus_order <- get_CS_counts(merged_DT,
        top_CS_only = top_CS_only
    )
    melt.dat <-
        locus_order %>%
        dplyr::mutate(Locus_UCS = paste0(Locus, "  (", UCS.CS_size, ")")) %>%
        reshape2:::melt.data.frame(
            measure.vars = grep(".CS_size$",
                colnames(locus_order),
                value = TRUE
            ),
            variable.name = "CS",
            value.name = "Credible Set size"
        ) %>%
        dplyr::mutate(Method = gsub(".CS_size$", "", CS)) %>%
        dplyr::arrange(Locus, Method) %>%
        dplyr::mutate(Method = factor(Method)) %>%
        subset(Method != "mean")
    melt.dat <- order_loci(dat = melt.dat, merged_DT = merged_DT)
    melt.dat[melt.dat$`Credible Set size` == 0 |
        is.na(melt.dat$`Credible Set size`), "Credible Set size"] <- NA


    ggplot(
        data = melt.dat,
        aes(y = Locus, x = `Credible Set size`, fill = Method)
    ) +
        geom_bar(stat = "identity", color = "white", size = .05) +
        geom_text(aes(label = `Credible Set size`),
            color = "grey20",
            size = 3, show.legend = FALSE,
            position = position_stack(vjust = .5)
        ) +
        geom_text(aes(x = sum(`Credible Set size`), label = Locus_UCS),
            size = 3, show.legend = FALSE,
            position = position_stack(vjust = 1)
        )

    ## Method-specific CS
    gg_CS <- ggplot(
        data = melt.dat,
        aes(y = Locus, x = `Credible Set size`, fill = Method)
    ) +
        geom_bar(stat = "identity", color = "white", size = .05) +
        labs(x = NULL, y = ylabel) +
        theme_bw() +
        theme(
            legend.position = "top",
            # axis.text.y = element_text(),
            # axis.title.y = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            legend.text = element_text(size = 8),
            legend.key.size = unit(.5, units = "cm")
        ) +
        guides(fill = guide_legend(
            nrow = legend_nrow,
            title.position = "top",
            title.hjust = .5
        ))
    if (show_numbers) {
        gg_CS <- gg_CS +
            geom_text(aes(label = `Credible Set size`),
                color = "grey20",
                size = 3, show.legend = FALSE,
                position = position_stack(vjust = .5)
            ) +
            geom_text(aes(x = sum(`Credible Set size`), label = Locus_UCS),
                size = 3, show.legend = FALSE,
                position = position_stack(vjust = 1)
            )
    }

    if (label_yaxis == FALSE) {
        gg_CS <- gg_CS + theme(axis.text.y = element_blank())
    }
    if (show_plot) print(gg_CS)
    return(list(
        plot = gg_CS,
        data = melt.dat
    ))
}
