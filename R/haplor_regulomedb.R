#' Download SNP-wise annotations from RegulomeDB
#'
#' @keywords internal
#' @family annotate
#' @source
#' \href{https://cran.r-project.org/web/packages/haploR/vignettes/haplor-vignette.html}{
#' HaploR}
#'
#' @keywords internal
#' @importFrom haploR queryRegulome
#' @importFrom data.table as.data.table rbindlist
haplor_regulomedb <- function(snp_list,
                              verbose = TRUE,
                              chunk_size = NA) {
    messager("+ Gathering annotation data from HaploReg...", v = verbose)
    # Break into smaller chunks
    snp_list <- unique(snp_list)
    if (is.na(chunk_size)) {
        chunk_size <- length(snp_list)
    }
    chunked_list <- split(snp_list, ceiling(seq_along(snp_list) / chunk_size))

    rDB_query <- lapply(names(chunked_list), function(i) {
        messager("++ Submitting chunk", i, "/", length(chunked_list))
        chunk <- chunked_list[[i]]
        rdb_query <- haploR::queryRegulome(
            query = chunk,
            timeout = 500,
            verbose = FALSE
        )
        return(data.table::as.data.table(rdb_query))
    }) %>% data.table::rbindlist()

    return(rDB_query)
}
