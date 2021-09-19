#' Tally tool-specific and union CS sizes
#'
#' @family summarise
#' @examples
#' \dontrun{
#' locus_order <- get_CS_counts(merged_DT = echodata::get_Nalls2019_merged())
#' }
#' @keywords internal
#' @importFrom dplyr %>% group_by summarise summarise_at vars funs n_distinct
#' @importFrom dplyr arrange
get_CS_counts <- function(merged_DT,
                          top_CS_only = FALSE) {
    Locus <- SNP <- Support <- . <- UCS.CS_size <- NULL

    UCS_count <- suppressMessages(
        merged_DT %>%
            dplyr::group_by(Locus, .drop = FALSE) %>%
            dplyr::summarise(UCS.CS_size = dplyr::n_distinct(SNP[Support > 0]))
    )

    if (top_CS_only) {
        tmp <- suppressWarnings(
            merged_DT %>%
                dplyr::group_by(Locus, .drop = FALSE) %>%
                dplyr::summarise_at(
                    .vars = dplyr::vars(dplyr::ends_with("CS")),
                    .funs = dplyr::funs(size = dplyr::n_distinct(SNP[. == 1],
                        na.rm = TRUE
                    ))
                )
        )
    } else {
        tmp <- suppressWarnings(
            merged_DT %>%
                dplyr::group_by(Locus, .drop = FALSE) %>%
                dplyr::summarise_at(
                    .vars = dplyr::vars(dplyr::ends_with("CS")),
                    .funs = dplyr::funs(size = dplyr::n_distinct(SNP[. > 0],
                        na.rm = TRUE
                    ))
                )
        )
    }
    locus_order <- tmp %>%
        base::merge(UCS_count, by = "Locus") %>%
        dplyr::arrange(-UCS.CS_size)
    # locus_order <- locus_order[!endsWith(colnames(locus_order), ".CS_size")]
    locus_order$Locus <- factor(locus_order$Locus,
        levels = locus_order$Locus,
        ordered = TRUE
    )
    return(data.frame(locus_order))
}
