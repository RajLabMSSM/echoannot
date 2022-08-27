#' Summarise \code{HaploR} annotations
#'
#' @keywords internal
#' @family annotate
#' @source
#' \href{https://cran.r-project.org/web/packages/haploR/vignettes/haplor-vignette.html}{
#' HaploR}
#' @importFrom data.table data.table .SD 
haplor_epigenetics_summary <- function(merged_results,
                                       tissue_list = c("BRN", "BLD"),
                                       # Chromatin_Marks
                                       epigenetic_variables =
                                           c(
                                               "Promoter_histone_marks",
                                               "Enhancer_histone_marks"
                                           )) {
    
    merged_results <- data.table::data.table(merged_results)
    summary_func <- function(ev) {
        boolean <- lapply(ev, function(x) {
            intersect(tissue_list, strsplit(as.character(x), ", ")[[1]]) |>
                length() > 0
        }) |> unlist()
        n_hits <- dim(merged_results[boolean, ])[1]
        Total <- dim(merged_results)[1]
        Percent_Total <- round(n_hits / Total * 100, 2)
        return(list(
            Hits = n_hits,
            Total = Total,
            Percent_Total = Percent_Total
        ))
    }
    epi_summary <- merged_results[, lapply(.SD, summary_func),
        .SDcols = epigenetic_variables
    ] |> t()
    colnames(epi_summary) <- c("Hits", "Total_SNPs", "Percent_Total")
    messager(epi_summary)
    return(epi_summary)
}
