#' Update CS cols
#'
#' Convert old column format: ".Credible_Set" => ".CS"
#' @examples
#' \dontrun{
#' finemap_DT <- update_cols(finemap_dat = echodata::BST1)
#' }
#' @keywords internal
update_cols <- function(finemap_dat) {
    colnames(finemap_dat) <- gsub("*.Credible_Set$", ".CS", colnames(finemap_dat))
    colnames(finemap_dat) <- gsub("*.Probability$", ".PP", colnames(finemap_dat))
    return(finemap_dat)
}
