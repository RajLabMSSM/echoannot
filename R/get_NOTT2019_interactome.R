
#' Brain cell type-specific enhancers, promoters, and interactomes
#'
#' Originally from \href{https://doi.org/10.1126/science.aay0793}{
#' Nott et al. (2019)}.
#' Specifically: \emph{aay0793-Nott-Table-S5.xlsx}.
#'
#' @family NOTT2019
#' @source \url{https://doi.org/10.1126/science.aay0793}
#' @source
#' \code{
#' file <- file.path(
#'     "~/Desktop/Fine_Mapping/echolocatoR/annotations",
#'     "NOTT2019/aay0793-Nott-Table-S5.xlsx"
#' )
#' sheets <- readxl::excel_sheets(file)
#' enh_prom_sheets <- grep("enhancers|promoters", sheets, value = TRUE)
#' other_sheets <- grep("enhancers|promoters", sheets,
#'     value = TRUE,
#'     invert = TRUE
#' )
#' NOTT2019_interactome <- lapply(other_sheets, function(s) {
#'     readxl::read_excel(file, sheet = s, skip = 2)
#' })
#' NOTT2019_interactome <- append(
#'     NOTT2019_interactome,
#'     lapply(enh_prom_sheets, function(s) {
#'         readxl::read_excel(file,
#'             sheet = s, skip = 2,
#'             col_names = c("chr", "start", "end")
#'         )
#'     })
#' )
#' names(NOTT2019_interactome) <- c(other_sheets, enh_prom_sheets)
#'
#' #### piggyback ####
#' tmp <- file.path(tempdir(), "NOTT2019_interactome.rds")
#' saveRDS(NOTT2019_interactome, tmp)
#' piggyback::pb_upload(
#'     file = tmp,
#'     repo = "RajLabMSSM/echoannot"
#' )
#' }
#' @export
#' @examples 
#' NOTT2019_interactome <- get_NOTT2019_interactome()
get_NOTT2019_interactome <- function() {
    tmp <- get_data(fname = "NOTT2019_interactome.rds")
    dat <- readRDS(tmp)
    return(dat)
}
