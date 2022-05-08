#' Order loci by UCS size, or alphabetically
#'
#' @examples
#' \dontrun{
#' merged_DT <- echodata::get_Nalls2019_merged()
#' merged_DT <- order_loci(
#'     dat = merged_DT,
#'     merged_DT = merged_DT,
#'     descending = FALSE
#' )
#' }
#' @keywords internal
#' @importFrom echodata get_CS_counts
order_loci <- function(dat,
                       merged_DT,
                       by_UCS_size = FALSE,
                       descending = TRUE,
                       verbose = FALSE) {
    if (by_UCS_size) {
        messager("+ Ordering loci by UCS size.", v = verbose)
        locus_order <- echodata::get_CS_counts(merged_DT)
        dat$Locus <- factor(dat$Locus,
            levels = locus_order$Locus,
            ordered = TRUE
        )
    } else {
        messager("+ Ordering loci alphabetically.", v = verbose)
        if (descending) {
            dat$Locus <- factor(dat$Locus,
                levels = rev(sort(unique(merged_DT$Locus))),
                ordered = TRUE
            )
        } else {
            dat$Locus <- factor(dat$Locus,
                levels = sort(unique(merged_DT$Locus)),
                ordered = TRUE
            )
        }
    }
    return(dat)
}
