#' Convert GRanges object to BED format and save
#'
#' @family XGR
#' @keywords internal
#' @importFrom data.table as.data.table fwrite
#' @importFrom parallel mclapply
#' @importFrom dplyr %>% select
#' @importFrom R.utils gzip
granges_to_bed <- function(GR.annotations,
                           output_path,
                           sep = "\t",
                           nThread = 1,
                           gzip = FALSE) {
    BED_paths <- parallel::mclapply(names(GR.annotations),
        function(name,
                 .gzip = gzip) {
            GR <- GR.annotations[[name]]
            BED <- data.table::as.data.table(GR) %>%
                dplyr::select(
                    chrom = seqnames,
                    chromStart = start,
                    chromEnd = end,
                    strand
                )
            BED_path <- file.path(output_path, paste0(
                gsub(":", "-", name),
                ".bed.txt"
            ))
            dir.create(dirname(BED_path), recursive = TRUE, showWarnings = FALSE)
            data.table::fwrite(BED, BED_path,
                sep = sep,
                col.names = FALSE, quote = FALSE
            )
            if (.gzip) {
                R.utils::gzip(BED_path, overwrite = TRUE)
                BED_path <- paste0(BED_path, ".gz")
            }
            return(BED_path)
        },
        mc.cores = nThread
    ) %>% unlist()
    return(BED_paths)
}
