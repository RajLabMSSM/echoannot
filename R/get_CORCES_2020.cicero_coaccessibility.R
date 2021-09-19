#' Cicero_coaccessibility from human brain tissue
#'
#' Cicero coaccessibility analysis for peaks that overlap SNPs derived
#' from analysis of scATAC-seq data.
#' Each row represents an individual peak identified from the feature
#' binarization analysis (see methods).
#'
#' Data originally from \href{https://doi.org/10.1038/s41588-020-00721-x}{
#' Corces et al. (bioRxiv)}, as of May 2020.
#' Specifically: \emph{STable10_Coacessibility_Peak_loop_connection},
#' \emph{Cicero Coaccessibility} sheet.
#' Peak_ID_Peak1 - A unique number that identifies the peak across
#' supplementary tables.
#'
#' \strong{Column dictionary}:
#' \describe{
#' \item{hg38_Chromosome_Peak1}{The hg38 chromosome of the first loop Peak.}
#' \item{hg38_Start_Peak1}{The hg38 start position of the first loop Peak.}
#' \item{hg38_Stop_Peak1}{The hg38 stop position of the first loop Peak.}
#' \item{Width_Peak1}{The width of the first loop Peak.}
#' \item{Peak_ID_Peak2}{A unique number that identifies the peak
#' across supplementary tables.}
#' \item{hg38_Chromosome_Peak2}{The hg38 chromosome of the second loop Peak.}
#' \item{hg38_Start_Peak2}{The hg38 start position of the second loop Peak.}
#' \item{hg38_Stop_Peak2}{The hg38 stop position of the second loop Peak.}
#' \item{Width_Peak2}{The width of the second loop Peak.}
#' \item{Coaccessibility}{The coaccessibility correlation
#' for the given peak pair.}
#' \item{Peak1_hasSNP}{A boolean variable determining whether
#' the first peak overlaps a SNP from our AD/PD GWAS analyses.}
#' \item{Peak2_hasSNP}{A boolean variable determining whether
#' the second peak overlaps a SNP from our AD/PD GWAS analyses.}
#' }
#' @family CORCES_2020
#' @source \url{https://doi.org/10.1038/s41588-020-00721-x}
#' @examples
#' \dontrun{
#' dat <- readxl::read_excel(
#'     file.path(
#'         "~/Desktop/Fine_Mapping/echolocatoR/annotations",
#'         "Coceres_2020/STable10_Coacessibility_Peak_loop_connection.xlsx"
#'     ),
#'     skip = 21, sheet = 2
#' )
#' CORCES_2020.cicero_coaccessibility <- data.table::data.table(dat)
#' 
#' #### piggyback ####  
#' tmp <- file.path(tempdir(),"CORCES_2020.cicero_coaccessibility.tsv.gz")
#' data.table::fwrite(CORCES_2020.cicero_coaccessibility,tmp,sep="\t")
#' piggyback::pb_upload(file = tmp,
#'                      repo = "RajLabMSSM/echoannot")
#' } 
#' @export
get_CORCES_2020.cicero_coaccessibility <- function(){
    tmp <- get_data(fname = "CORCES_2020.cicero_coaccessibility.tsv.gz")
    dat <- data.table::fread(tmp, nThread = 1)
    return(dat)
}
