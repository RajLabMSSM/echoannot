#' Download SNP-wise annotations from RegulomeDB
#'
#' @param verbose Print messages. 
#' @inheritParams haploR::queryRegulome
#' @keywords internal
#' @family annotate
#' @source
#' \href{https://cran.r-project.org/web/packages/haploR/vignettes/haplor-vignette.html}{
#' HaploR}
#'
#' @keywords internal 
#' @importFrom data.table as.data.table rbindlist
haplor_regulomedb <- function(snp_list,
                              timeout = 500,
                              chunk_size = NA,
                              verbose = TRUE) {
    requireNamespace("haploR")
    regulome.start <- NULL;
    
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
            timeout = timeout
        )
        data.table::data.table(rdb_query$summary)[,-("chrom")]
    }) |> data.table::rbindlist() 
    data.table::setnames(rDB_query, 
                         c("rsids","start","end"), 
                         c("rsID","regulome.start","regulome.end"), 
                         skip_absent = TRUE)
    rDB_query <- dplyr::relocate(rDB_query, 
                                 regulome.start, 
                                 .before = "regulome.end")
    return(rDB_query)
}
