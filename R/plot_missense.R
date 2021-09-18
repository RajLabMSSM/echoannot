

#' Plot any missense variants
#'
#' @family annotate
#' @examples
#' \dontrun{
#' merged_DT <- echodata::Nalls2019_merged
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
    . <- SNP <- Missense <- Locus <- dummy <- NULL

    locus_order <- get_CS_counts(merged_DT = merged_DT)
    annotated_DT <- annotate_missense(
        merged_DT = merged_DT,
        snp_filter = snp_filter
    )
    dat_melt <-
        data.table::setDT(annotated_DT)[,
            .(Missense = n_distinct(
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

    gg_missense <- ggplot(data = dat_melt, aes(x = dummy, y = Locus, fill = Missense)) +
        geom_tile(show.legend = show.legend, alpha = .7) +
        # scale_fill_continuous(na.value = "transparent") +
        # scale_fill_gradient(low = scales::alpha("blue",.7),
        #                     high = scales::alpha("red",.7),
        #                     na.value = "transparent",
        #                     n.breaks=max(dat_melt$Missense, na.rm = T)) +
        # scale_fill_fermenter(palette = "Spectral", na.value = "transparent") +
        scale_fill_viridis_c(na.value = "transparent") +
        theme_bw() +
        labs(x = NULL, fill = "Missense\nmutations") +
        theme(
            axis.text.x = element_text(angle = 40, hjust = 1),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            legend.position = "top",
            legend.title.align = .5,
            legend.text.align = .5,
            legend.box = "horizontal",
            legend.key = element_rect(colour = "gray60")
        )
    if (show_numbers) {
        gg_missense <- gg_missense +
            geom_text(aes(label = Missense), color = "grey70")
    }
    if (label_yaxis == FALSE) {
        gg_missense <- gg_missense + theme(axis.text.y = element_blank()) +
            labs(y = NULL)
    }
    if (show_plot) print(gg_missense)
    return(list(
        plot = gg_missense,
        data = dat_melt
    ))
}
