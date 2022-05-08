#' Get widow limits 
#' 
#' Get genomic window size limits for a locus plot.
#' @param dat Data.
#' @param index_as_center Use the index/lead SNP 
#' (the SNP with the smallest P-value) as the center point for the window.
#' @param zoom Zoom into the center of the locus when plotting
#' (without editing the fine-mapping results file).
#' You can provide either:
#' \itemize{
#' \item{The size of your plot window in terms of basepairs
#' (e.g. \code{zoom=50000} for a 50kb window)}.
#' \item{How much you want to zoom in (e.g. \code{zoom="1x"}
#' for the full locus, \code{zoom="2x"}
#' for 2x zoom into the center of the locus, etc.)}.
#' }
#' You can pass a list of window sizes (e.g. \code{c(50000,100000,500000)})
#' to automatically generate
#' multiple views of each locus.
#' This can even be a mix of different style inputs: e.g.
#'  \code{c("1x","4.5x",25000)}.
#' @param genomic_units Which genomic units to return window limits in.
#' @param verbose Print messages.
#' 
#' @family plot
#' @export
#' @examples
#' dat <- echodata::BST1
#' xlims <- get_window_limits(dat = dat, zoom = 50000)
#' xlims <- get_window_limits(dat = dat, zoom = "all")
#' xlims <- get_window_limits(dat = dat, zoom = "5x")
get_window_limits <- function(dat,
                              index_as_center = TRUE,
                              zoom = NULL,
                              genomic_units = "Mb",
                              verbose = TRUE) {
    # zoom <- c("all","1x","4x",5000);
    # zoom <- c("5000");
    # dat=echolocatoR::BST1;
    # index_as_center=T; genomic_units="Mb"; verbose=T;
    
    leadSNP <- NULL;
    zoom <- if (is.null(zoom)) "1x" else zoom
    zoom[!is.na(zoom)] <- zoom
    zoom[!is.null(zoom)] <- zoom
    # Iterate over list of zooms
    xlims_list <- lapply(zoom, function(pz,
                                         .dat = dat,
                                         .index_as_center = index_as_center,
                                         .genomic_units = genomic_units,
                                         .verbose = verbose) {
        messager("+ Inferring genomic limits for window:", pz, v = .verbose)
        # Zoom #x as  input
        if (.index_as_center) {
            middle_pos <- subset(.dat, leadSNP)$POS[1]
        } else {
            ## Lead Pos isn't always dead middle if manual xlims
            ## were used during querying
            middle_pos <- .dat[as.numeric(round(nrow(.dat)) / 2), ]$POS
        }
        if (grepl("x$", tolower(pz))) {
            if (tolower(pz) == "1x") {
                min_limit <- min(.dat$POS, na.rm = TRUE)
                max_limit <- max(.dat$POS, na.rm = TRUE)
            } else {
                total_bp_span <- (max(.dat$POS, na.rm = TRUE) -
                    min(.dat$POS, na.rm = TRUE))
                new_window <- total_bp_span / as.numeric(gsub("x", "", pz))
                # Prevent extending beyond the borders of the data
                # (producing blank space)
                min_limit <- middle_pos - as.integer(new_window / 2)
                max_limit <- middle_pos + as.integer(new_window / 2)
            }
        } else {
            # Basepairs as input
            if (is.null(pz)) {
                min_limit <- min(.dat$POS, na.rm = TRUE)
                max_limit <- max(.dat$POS, na.rm = TRUE)
            } else {
                # 'all' as input
                if (pz == "all") {
                    min_limit <- min(.dat$POS, na.rm = TRUE)
                    max_limit <- max(.dat$POS, na.rm = TRUE)
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
    }) %>% `names<-`(zoom)

    # For backwards compatibility
    if (length(xlims_list) == 1) {
        return(xlims_list[[1]])
    } else {
        return(xlims_list)
    }
}
