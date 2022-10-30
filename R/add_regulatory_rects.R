add_regulatory_rects <- function(NOTT.interact_trk,
                                 regions,
                                 genomic_units,
                                 max.height,
                                 interact_y,
                                 color_dict,
                                 verbose = TRUE){
    requireNamespace("ggplot2")
    messager("++ Adding enhancer/promoter rectangles",
             v = verbose)
    rect_height <- max.height / 10
    regions$y <- ifelse(regions$Element == "promoters",
                        0 + (rect_height * 2),
                        ifelse(regions$Element == "enhancers", 0,
                               interact_y + rect_height
                        )
    )
    regions$Element <- factor(regions$Element,
                              levels = c("enhancers", "promoters", "anchors"),
                              ordered = TRUE
    )
    #### Create plot ####
    NOTT.interact_trk <- suppressMessages(
        NOTT.interact_trk +
            ggbio::geom_rect(
                data = regions,
                stat = "identity",
                rect.height = rect_height,
                ggplot2::aes_string(y = "y", fill = "Element"),
                alpha = .7, inherit.aes = FALSE,
                color = "transparent"
                # hjust = 0
            ) +
            ggplot2::scale_fill_manual(values = color_dict) +
            # geom_point(data = data.frame(regions),
            #            aes(x = middle, y = 0, color = Element),size = 0.5,
            #            inherit.aes = F, alpha = 0.7) +
            ggplot2::scale_color_manual(values = color_dict) +
            ggplot2::geom_hline(
                yintercept = Inf, alpha = 0.2,
                show.legend = FALSE
            ) +
            ggplot2::scale_y_reverse()
    )
    
    if (genomic_units == "Mb") {
        NOTT.interact_trk <- ggbio_to_mb(gg = NOTT.interact_trk,
                                         verbose = verbose) 
    }
    return(NOTT.interact_trk)
}