#' get_max_histogram_height
#'
#' @keywords internal
#' @importFrom methods is
#' @importFrom ggplot2 ggplot_build
#' @importFrom DescTools RoundTo
get_max_histogram_height <- function(gg,
                                     round_to = NULL,
                                     verbose = TRUE) {
    if (echodata::is_ggbio(gg)) {
        gg <- gg@ggplot
    }
    messager("+ Calculating max histogram height", v = verbose)
    dat <- ggplot2::ggplot_build(gg)$data[[1]]
    max_height <- max(dat$ymax, na.rm = TRUE)
    if (!is.null(round_to)) {
        max_height <- DescTools::RoundTo(max_height, round_to)
    }
    return(max_height)
}
