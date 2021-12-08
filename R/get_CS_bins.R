#' Count bins of tool-specific and union CS sizes
#'
#' @family summarise
#' @examples
#' \dontrun{
#' bin_counts <- get_CS_bins(merged_DT = echodata::get_Nalls2019_merged())
#' }
#' @keywords internal
#' @importFrom dplyr %>% mutate group_by case_when
#' @importFrom data.table melt.data.table data.table
get_CS_bins <- function(merged_DT) {
    
    variable <- Method <- NULL;
    locus_order <- get_CS_counts(merged_DT = merged_DT)
    max_CS_size <- sapply(locus_order[, -1], max, na.rm = TRUE) %>% max()
    labels <- c("0", "1", "2-4", "5-7", "8-10", "11-15", "16+")
    bin_counts <-
        data.table::data.table(locus_order) %>% 
        data.table::melt.data.table(
            measure.vars = grep("*_size$",
                colnames(locus_order),
                value = TRUE
            ),
            value.name = "CS_size"
        ) %>%
        dplyr::mutate(Method = gsub("\\.CS_size$|_size$", "", variable)) %>%
        dplyr::group_by(Method, .drop = FALSE) %>%
        dplyr::mutate(bin = dplyr::case_when(
            CS_size == 0 ~ labels[1],
            CS_size == 1 ~ labels[2],
            CS_size > 1 & CS_size <= 4 ~ labels[3],
            CS_size > 4 & CS_size <= 7 ~ labels[4],
            CS_size > 7 & CS_size <= 10 ~ labels[5],
            CS_size > 10 & CS_size <= 15 ~ labels[6],
            CS_size >= 16 ~ labels[7]
        ))
    bin_counts$bin <- factor(bin_counts$bin,
        levels = rev(labels),
        ordered = TRUE
    )
    return(data.frame(bin_counts))
}
