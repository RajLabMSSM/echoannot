
#' Brain cell type-specific enhancers, promoters, and interactomes
#'
#' Originally from \href{https://science.sciencemag.org/content/366/6469/1134}{
#' Nott et al. (2019)}.
#' Specifically: \emph{aay0793-Nott-Table-S5.xlsx}.
#'
#' @family NOTT_2019
#' @source \url{https://science.sciencemag.org/content/366/6469/1134}
#' @examples
#' \dontrun{
#' file <- file.path(
#'     "~/Desktop/Fine_Mapping/echolocatoR/annotations",
#'     "Nott_2019/aay0793-Nott-Table-S5.xlsx"
#' )
#' sheets <- readxl::excel_sheets(file)
#' enh_prom_sheets <- grep("enhancers|promoters", sheets, value = TRUE)
#' other_sheets <- grep("enhancers|promoters", sheets,
#'     value = TRUE,
#'     invert = TRUE
#' )
#' NOTT_2019.interactome <- lapply(other_sheets, function(s) {
#'     readxl::read_excel(file, sheet = s, skip = 2)
#' })
#' NOTT_2019.interactome <- append(
#'     NOTT_2019.interactome,
#'     lapply(enh_prom_sheets, function(s) {
#'         readxl::read_excel(file,
#'             sheet = s, skip = 2,
#'             col_names = c("chr", "start", "end")
#'         )
#'     })
#' )
#' names(NOTT_2019.interactome) <- c(other_sheets, enh_prom_sheets)
#' 
#' #### piggyback ####  
#' tmp <- file.path(tempdir(),"NOTT_2019.interactome.rds")
#' saveRDS(NOTT_2019.interactome,tmp)
#' piggyback::pb_upload(file = tmp,
#'                      repo = "RajLabMSSM/echoannot")
#' } 
#' @export
get_NOTT_2019.interactome <- function(){
    tmp <- get_data(fname = "NOTT_2019.interactome.rds")
    dat <- readRDS(tmp)
    return(dat)
}