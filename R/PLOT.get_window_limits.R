#' Get window size limits for plot
#'
#' @family plot
#' @keywords internal
#' @examples
#' data("BST1")
#' xlims <- PLOT.get_window_limits(finemap_dat = BST1, plot.zoom = 50000)
#' xlims <- PLOT.get_window_limits(finemap_dat = BST1, plot.zoom = "all")
#' xlims <- PLOT.get_window_limits(finemap_dat = BST1, plot.zoom = "5x")
PLOT.get_window_limits <- function(finemap_dat,
                                   index_as_center = TRUE,
                                   plot.zoom = NULL,
                                   genomic_units = "Mb",
                                   verbose = TRUE) {
    # plot.zoom <- c("all","1x","4x",5000);
    # plot.zoom <- c("5000");
    # finemap_dat=echolocatoR::BST1;
    # index_as_center=T; genomic_units="Mb"; verbose=T;
    #
    plot.zoom <- if (is.null(plot.zoom)) "1x" else plot.zoom
    plot.zoom[!is.na(plot.zoom)] <- plot.zoom
    plot.zoom[!is.null(plot.zoom)] <- plot.zoom
    # Iterate over list of zooms
    xlims_list <- lapply(plot.zoom, function(pz,
                                             .finemap_dat = finemap_dat,
                                             .index_as_center = index_as_center,
                                             .genomic_units = genomic_units,
                                             .verbose = verbose) {
        printer("+ Inferring genomic limits for window:", pz, v = .verbose)
        # Zoom #x as  input
        if (.index_as_center) {
            middle_pos <- subset(.finemap_dat, leadSNP)$POS[1]
        } else {
            # Lead Pos isn't always dead middle if manual xlims were used during querying
            middle_pos <- .finemap_dat[as.numeric(round(nrow(.finemap_dat)) / 2), ]$POS
        }

        if (grepl("x$", tolower(pz))) {
            if (tolower(pz) == "1x") {
                min_limit <- min(.finemap_dat$POS, na.rm = TRUE)
                max_limit <- max(.finemap_dat$POS, na.rm = TRUE)
            } else {
                total_bp_span <- (max(.finemap_dat$POS, na.rm = TRUE) -
                    min(.finemap_dat$POS, na.rm = TRUE))
                new_window <- total_bp_span / as.numeric(gsub("x", "", pz))
                # Prevent extending beyond the borders of the data
                # (producing blank space)
                min_limit <- middle_pos - as.integer(new_window / 2)
                max_limit <- middle_pos + as.integer(new_window / 2)
            }
        } else {
            # Basepairs as input
            if (is.null(pz)) {
                min_limit <- min(.finemap_dat$POS, na.rm = TRUE)
                max_limit <- max(.finemap_dat$POS, na.rm = TRUE)
            } else {
                # 'all' as input
                if (pz == "all") {
                    min_limit <- min(.finemap_dat$POS, na.rm = TRUE)
                    max_limit <- max(.finemap_dat$POS, na.rm = TRUE)
                } else {
                    min_limit <- middle_pos - as.integer(as.numeric(pz) / 2)
                    max_limit <- middle_pos + as.integer(as.numeric(pz) / 2)
                }
            }
        }
        xlims <- c(min_limit, max_limit)
        if (.genomic_units == "Mb") {
            xlims <- xlims / 1000000
        }
        return(xlims)
    }) %>% `names<-`(plot.zoom)

    # For backwards compatibility
    if (length(xlims_list) == 1) {
        return(xlims_list[[1]])
    } else {
        return(xlims_list)
    }
}
