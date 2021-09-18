#-------Corces et al. (bioRxiv) data --------


#' bulkATACseq peaks from human brain tissue
#'
#' Each row represents an individual peak identified in the bulk ATAC-seq data.
#'
#' Data originally from \href{https://doi.org/10.1038/s41588-020-00721-x}{
#' Corces et al. (bioRxiv)}, as of May 2020.
#' Specifically: \emph{STable2_Features_bulkATAC-seq_Peaks}
#'
#' @family CORCES_2020
#' @source \url{https://doi.org/10.1038/s41588-020-00721-x}
#' @examples
#' \dontrun{
#' dat <- readxl::read_excel(
#'     file.path(
#'         "~/Desktop/Fine_Mapping/echolocatoR",
#'         "annotations/Coceres_2020",
#'         "STable2_Features_bulkATAC-seq_Peaks.xlsx"
#'     ),
#'     skip = 18
#' )
#' CORCES_2020.bulkATACseq_peaks <- data.table::data.table(dat)
#' usethis::use_data(CORCES_2020.bulkATACseq_peaks, overwrite = TRUE)
#' }
"CORCES_2020.bulkATACseq_peaks"


#' scATACseq peaks from human brain tissue
#'
#' Each row represents an individual peak identified in
#' the single-cell ATAC-seq data.
#'
#' Data originally from \href{https://doi.org/10.1038/s41588-020-00721-x}{
#' Corces et al. (bioRxiv)}, as of May 2020.
#' Specifically: \emph{STable5_Features_scATAC-seq_Peaks_all}
#' @family CORCES_2020
#' @source \url{https://doi.org/10.1038/s41588-020-00721-x}
#' @examples
#' \dontrun{
#' dat <- readxl::read_excel(
#'     file.path(
#'         "~/Desktop/Fine_Mapping/echolocatoR/annotations",
#'         "Coceres_2020/STable5_Features_scATAC-seq_Peaks_all.xlsx"
#'     ),
#'     skip = 18
#' )
#' CORCES_2020.scATACseq_peaks <- data.table::data.table(dat)
#' usethis::use_data(CORCES_2020.scATACseq_peaks, overwrite = TRUE)
#' }
"CORCES_2020.scATACseq_peaks"


#' scATACseq cell type-specific peaks from human brain tissue
#'
#' Each row represents an individual peak identified from the feature
#'  binarization analysis (see methods).
#'
#' Data originally from \href{https://doi.org/10.1038/s41588-020-00721-x}{
#' Corces et al. (bioRxiv)}, as of May 2020.
#' Specifically: \emph{STable6_Features_scATAC-seq_celltype_Peaks}
#'
#' @family CORCES_2020
#' @source \url{https://doi.org/10.1038/s41588-020-00721-x}
#' @examples
#' \dontrun{
#' dat <- readxl::read_excel(
#'     file.path(
#'         "~/Desktop/Fine_Mapping/echolocatoR/annotations",
#'         "Coceres_2020/STable6_Features_scATAC-seq_celltype_Peaks.xlsx"
#'     ),
#'     skip = 15
#' )
#' CORCES_2020.scATACseq_celltype_peaks <- data.table::data.table(dat)
#' usethis::use_data(CORCES_2020.scATACseq_celltype_peaks, overwrite = TRUE)
#' }
"CORCES_2020.scATACseq_celltype_peaks"


#' FitHiChIP loop calls from human brain tissue
#'
#' FitHiChIP loop calls that overlap SNPs derived from analysis
#'  of H3K27ac HiChIP data.
#' Each row represents an individual peak identified from the feature
#' binarization analysis (see methods).
#'
#' Data originally from \href{https://doi.org/10.1038/s41588-020-00721-x}{
#' Corces et al. (bioRxiv)}, as of May 2020.
#' Specifically: \emph{STable10_Coacessibility_Peak_loop_connection},
#' \emph{HiChIP FitHiChIP Loop Calls} sheet.
#'
#' \strong{Column dictionary}
#' \describe{
#' \item{hg38_Chromosome_Anchor1}{The hg38 chromosome of the first loop Anchor.}
#' \item{hg38_Start_Anchor1}{The hg38 start position of the first loop Anchor.}
#' \item{hg38_Stop_Anchor1}{The hg38 stop position of the first loop Anchor.}
#' \item{Width_Anchor1}{The width of the first loop Anchor.}
#' \item{hg38_Chromosome_Anchor2}{
#' The hg38 chromosome of the second loop Anchor.}
#' \item{hg38_Start_Anchor2}{The hg38 start position of the second loop Anchor.}
#' \item{hg38_Stop_Anchor2}{The hg38 stop position of the second loop Anchor.}
#' \item{Width_Anchor2}{The width of the second loop Anchor.}
#' \item{Score}{The -log10(q-value) of the loop call from FitHiChIP.}
#' \item{Anchor1_hasSNP}{A boolean variable determining whether the first
#' anchor overlaps a SNP from our AD/PD GWAS analyses.}
#' \item{Anchor2_hasSNP}{A boolean variable determining whether the second
#' anchor overlaps a SNP from our AD/PD GWAS analyses.}
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
#'     skip = 19, sheet = 1
#' )
#' CORCES_2020.HiChIP_FitHiChIP_loop_calls <- data.table::data.table(dat)
#' usethis::use_data(CORCES_2020.HiChIP_FitHiChIP_loop_calls)
#' }
"CORCES_2020.HiChIP_FitHiChIP_loop_calls"


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
#' usethis::use_data(CORCES_2020.cicero_coaccessibility)
#' }
"CORCES_2020.cicero_coaccessibility"



# --------------Nott et al. (2019) -------------


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
#' usethis::use_data(NOTT_2019.interactome, overwrite = TRUE)
#' }
"NOTT_2019.interactome"




#' Brain cell type-specific interactomes with superenhancers
#'
#' Originally from \href{https://science.sciencemag.org/content/366/6469/1134}{
#' Nott et al. (2019)}.
#' Specifically: \emph{aay0793-Nott-Table-S6.xlsx}.
#'
#' @family NOTT_2019
#' @source \url{https://science.sciencemag.org/content/366/6469/1134}
#' @examples
#' \dontrun{
#' NOTT_2019.superenhancer_interactome <- data.table::data.table(
#'     readxl::read_excel(
#'         file.path(
#'             "~/Desktop/Fine_Mapping/echolocatoR",
#'             "annotations/Nott_2019/aay0793-Nott-Table-S6.xlsx"
#'         ),
#'         skip = 2
#'     )
#' )
#' usethis::use_data(NOTT_2019.superenhancer_interactome)
#' }
"NOTT_2019.superenhancer_interactome"


#' Metadata and links to data
#'
#' Metadata for cell type-specific epigenomic bigWig files hosted
#'  on UCSC Genome Browser.
#' bigWig files contain the genomic ranges from each epigenomic assay,
#' as well as a Score column which describes the peaks of the aggregate reads.
#' @family NOTT_2019
#' @source \url{https://science.sciencemag.org/content/366/6469/1134}
#' @examples
#' \dontrun{
#' NOTT_2019.bigwig_metadata <- data.table::data.table(
#'     readxl::read_excel(
#'         file.path(
#'             "~/Desktop/Fine_Mapping/echolocatoR/annotations",
#'             "Nott_2019/Nott_2019.snEpigenomics.xlsx"
#'         )
#'     )
#' )
#' usethis::use_data(NOTT_2019.bigwig_metadata, overwrite = TRUE)
#' }
"NOTT_2019.bigwig_metadata"
