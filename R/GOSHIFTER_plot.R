#' GoShifter p-value histograms
#'
#' Plot histograms of GoShifter p-values for each annotation source.
#' Two p-value columns are shown side-by-side: the manually computed
#' \code{pval} and the GoShifter-reported \code{GS_pval}.
#'
#' @param GS_results A \code{data.table} of GoShifter results containing
#'   columns \code{Annotation}, \code{pval}, and \code{GS_pval}.
#'
#' @returns A \code{ggplot} object (also printed).
#'
#' @family GOSHIFTER
#' @export
#' @importFrom ggplot2 ggplot aes geom_histogram theme_bw
#' @importFrom data.table melt.data.table
#' @importFrom rlang .data
#' @examples
#' \dontrun{
#' GS_results <- data.table::data.table(
#'     Annotation = rep("E001", 10),
#'     pval = runif(10),
#'     GS_pval = runif(10)
#' )
#' GOSHIFTER_histograms_pvals(GS_results)
#' }
GOSHIFTER_histograms_pvals <- function(GS_results) {

    GS <- unique(GS_results[, c("Annotation", "pval", "GS_pval")])
    GS <- data.table::melt.data.table(
        GS,
        id.vars = "Annotation",
        variable.name = "P.value Source",
        value.name = "P.value"
    )
    hst <- ggplot2::ggplot(
        data = GS,
        ggplot2::aes(x = .data[["P.value"]],
                     fill = .data[["P.value Source"]])
    ) +
        ggplot2::geom_histogram(alpha = 0.5) +
        ggplot2::theme_bw()
    print(hst)
    return(hst)
}


#' GoShifter SNP-group histograms
#'
#' Plot histograms of GoShifter p-values faceted by SNP group.
#'
#' @param GS_groups A \code{data.table} of GoShifter results containing
#'   columns \code{SNP_group}, \code{Annotation}, \code{pval}, and
#'   \code{GS_pval}.
#' @param show_plot If \code{TRUE}, print the plot (default \code{TRUE}).
#'
#' @returns A \code{ggplot} object.
#'
#' @family GOSHIFTER
#' @export
#' @importFrom ggplot2 ggplot aes geom_histogram
#' @examples
#' \dontrun{
#' GS_groups <- data.table::data.table(
#'     SNP_group = rep(c("A", "B"), each = 5),
#'     Annotation = "E001",
#'     pval = runif(10),
#'     GS_pval = runif(10)
#' )
#' GOSHIFTER_histograms_SNPgroups(GS_groups)
#' }
GOSHIFTER_histograms_SNPgroups <- function(GS_groups,
                                           show_plot = TRUE) {

    GS <- unique(GS_groups[, c("SNP_group", "Annotation",
                                "pval", "GS_pval")])
    hst <- ggplot2::ggplot(
        GS,
        ggplot2::aes(x = .data[["pval"]],
                     fill = .data[["SNP_group"]])
    ) +
        ggplot2::geom_histogram(alpha = 0.7)
    if (show_plot) print(hst)
    return(hst)
}


#' GoShifter enrichment heatmap
#'
#' Create an interactive heatmap of GoShifter enrichment results for
#' significant tests (p <= 0.05), using \pkg{heatmaply}.
#'
#' @param GS_groups A \code{data.table} of GoShifter results containing
#'   columns \code{pval}, \code{Epigenome.name}, \code{chromatin_state},
#'   \code{SNP_group}, and \code{mean_enrichment}.
#' @param show_plot If \code{TRUE}, print the heatmap (default
#'   \code{TRUE}).
#'
#' @returns A \pkg{heatmaply} heatmap object.
#'
#' @family GOSHIFTER
#' @export
#' @examples
#' \dontrun{
#' GS_groups <- data.table::data.table(
#'     pval = c(0.01, 0.03, 0.8),
#'     Epigenome.name = c("A", "B", "C"),
#'     chromatin_state = "TssA",
#'     SNP_group = "group1",
#'     mean_enrichment = c(0.5, 0.3, 0.1)
#' )
#' GOSHIFTER_heatmap(GS_groups)
#' }
GOSHIFTER_heatmap <- function(GS_groups,
                              show_plot = TRUE) {

    if (!requireNamespace("reshape2", quietly = TRUE)) {
        stop("Package 'reshape2' is required for GOSHIFTER_heatmap(). ",
             "Install it with: install.packages('reshape2')",
             call. = FALSE)
    }
    if (!requireNamespace("heatmaply", quietly = TRUE)) {
        stop("Package 'heatmaply' is required for GOSHIFTER_heatmap(). ",
             "Install it with: install.packages('heatmaply')",
             call. = FALSE)
    }

    sig_tests <- subset(GS_groups, GS_groups$pval <= 0.05)
    mat <- reshape2::acast(
        sig_tests,
        Epigenome.name ~ chromatin_state + SNP_group,
        value.var = "mean_enrichment",
        fun.aggregate = mean,
        drop = FALSE,
        fill = 0
    )
    hm <- heatmaply::heatmaply(mat, dendrogram = "row", k_row = 3)
    if (show_plot) print(hm)
    return(hm)
}
