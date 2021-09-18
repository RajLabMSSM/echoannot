#' Download SNP-wise annotations from HaploReg
#'
#' @keywords internal
#' @family annotate
#' @source
#' \href{https://cran.r-project.org/web/packages/haploR/vignettes/haplor-vignette.html}{
#' HaploR}
#'
#' @keywords internal
#' @importFrom data.table as.data.table rbindlist
haplor_haploreg <- function(snp_list, verbose = T, chunk_size = NA) {
    messager("+ Gathering annotation data from HaploReg...", v = verbose)
    # Break into smaller chunks
    snp_list <- unique(snp_list)
    if (is.na(chunk_size)) {
        chunk_size <- length(snp_list)
    }
    chunked_list <- split(snp_list, ceiling(seq_along(snp_list) / chunk_size))

    HR_query <- lapply(names(chunked_list), function(i) {
        messager("++ Submitting chunk", i, "/", length(chunked_list))
        chunk <- chunked_list[[i]]
        HR_query <- haploR::queryHaploreg(
            query = chunk,
            file = NULL,
            study = NULL,
            ldThresh = NA,
            ldPop = "EUR",
            epi = "vanilla",
            cons = "siphy",
            genetypes = "gencode",
            url = "https://pubs.broadinstitute.org/mammals/haploreg/haploreg.php",
            timeout = 500,
            encoding = "UTF-8",
            verbose = FALSE
        )

        return(data.table::as.data.table(HR_query))
    }) %>% data.table::rbindlist()

    return(HR_query)
}
