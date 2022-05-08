#' Brain cell type-specific interactomes with superenhancers
#'
#' Originally from \href{https://doi.org/10.1126/science.aay0793}{
#' Nott et al. (2019)}.
#' Specifically: \emph{aay0793-Nott-Table-S6.xlsx}.
#'
#' @family NOTT2019
#' @source \url{https://doi.org/10.1126/science.aay0793}
#' @examples
#' \dontrun{
#' NOTT2019_superenhancer_interactome <- data.table::data.table(
#'     readxl::read_excel(
#'         file.path(
#'             "~/Desktop/Fine_Mapping/echolocatoR",
#'             "annotations/NOTT2019/aay0793-Nott-Table-S6.xlsx"
#'         ),
#'         skip = 2
#'     )
#' )
#'
#' #### piggyback ####
#' tmp <- file.path(tempdir(), "NOTT2019_superenhancer_interactome.tsv.gz")
#' data.table::fwrite(NOTT2019_superenhancer_interactome, tmp, sep = "\t")
#' piggyback::pb_upload(
#'     file = tmp,
#'     repo = "RajLabMSSM/echoannot"
#' )
#' }
#' @export
get_NOTT2019_superenhancer_interactome <- function() {
    tmp <- get_data(fname = "NOTT2019_superenhancer_interactome.tsv.gz")
    dat <- data.table::fread(tmp, nThread = 1)
    return(dat)
}
