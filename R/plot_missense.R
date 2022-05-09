#' Plot any missense variants
#' 
#' Plot any missense variants in fine-mapped data.
#' @param label_yaxis Whether to label the y-axis.
#' @param x_label x-axis title.
#' @param show_numbers Whether to plot the numeric values or not.
#' @inheritParams super_summary_plot
#' @inheritParams ggplot2::geom_tile
#' 
#' @family annotate
#' @export
#' @importFrom echodata get_CS_counts
#' @importFrom methods show
#' @examples
#' \dontrun{
#' merged_DT <- echodata::get_Nalls2019_merged()
#' gg_missense <- plot_missense(
#'     merged_DT = merged_DT,
#'     snp_filter = "Support>0"
#' )
#' gg_missense <- plot_missense(
#'     merged_DT = merged_DT,
#'     snp_filter = "Consensus_SNP==TRUE"
#' )
#' }
plot_missense <- function(merged_DT,
                          snp_filter = "Support>0",
                          label_yaxis = FALSE,
                          x_label = "UCS missense\nmutations",
                          show.legend = TRUE,
                          show_numbers = FALSE,
                          show_plot = TRUE) {
    
    requireNamespace("ggplot2")
    . <- SNP <- Missense <- Locus <- dummy <- NULL;
    locus_order <- echodata::get_CS_counts(merged_DT = merged_DT)
    annotated_DT <- annotate_missense(
        merged_DT = merged_DT,
        snp_filter = snp_filter
    )
    dat_melt <-
        data.table::setDT(annotated_DT)[,
            .(Missense = dplyr::n_distinct(
                SNP[Missense == TRUE],
                na.rm = TRUE
            )),
            by = c("Locus")
        ] %>%
        dplyr::mutate(
            Locus = factor(Locus,
                levels = unique(locus_order$Locus),
                ordered = TRUE
            ),
            Missense = as.integer(Missense)
        )
    dat_melt$dummy <- x_label
    dat_melt[dat_melt$Missense == 0, "Missense"] <- NA

    gg_missense <- ggplot2::ggplot(data = dat_melt, 
                                   ggplot2::aes(x = dummy, y = Locus, 
                                                fill = Missense)) +
        ggplot2::geom_tile(show.legend = show.legend, alpha = .7) +
        ggplot2::scale_fill_viridis_c(na.value = "transparent") +
        ggplot2::theme_bw() +
        ggplot2::labs(x = NULL, fill = "Missense\nmutations") +
        ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = 40, hjust = 1),
            panel.grid.major.x = ggplot2::element_blank(),
            panel.grid.minor.x = ggplot2::element_blank(),
            legend.position = "top",
            legend.title.align = .5,
            legend.text.align = .5,
            legend.box = "horizontal",
            legend.key = ggplot2::element_rect(colour = "gray60")
        )
    if (show_numbers) {
        gg_missense <- gg_missense +
            ggplot2::geom_text(
                ggplot2::aes(label = Missense), color = "grey70")
    }
    if (label_yaxis == FALSE) {
        gg_missense <- gg_missense + 
            ggplot2::theme(axis.text.y = ggplot2::element_blank()) +
            ggplot2::labs(y = NULL)
    }
    if (show_plot) print(gg_missense)
    return(list(
        plot = gg_missense,
        data = dat_melt
    ))
}
