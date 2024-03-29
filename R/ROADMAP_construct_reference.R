#' Gather Roadmap annotation metadata
#'
#' @param ref_path Where the ROADMAP metadata is stored.
#' @param keyword_query Search all columns in the Roadmap annotations metadata
#' and only query annotations that contain your keywords.
#' Can provide multiple keywords in list form:
#' \code{c("placenta","liver","monocytes")}.
#' @param limit_files Limit the number of annotation files queried
#' (for faster testing).
#' @param verbose Print messages.
#' @family ROADMAP
#' 
#' @export
#' @importFrom data.table transpose fread
#' @examples
#' ref <- ROADMAP_construct_reference(keyword_query = c(
#'     "placenta",
#'     "liver",
#'     "monocytes"))
ROADMAP_construct_reference <- function(ref_path =
                                            system.file(
                                                "extdata/ROADMAP",
                                                "ROADMAP_Epigenomic.js",
                                                package = "echoannot"
                                            ),
                                        limit_files = NULL,
                                        keyword_query = NULL,
                                        verbose = TRUE) {
    # %like% is from data.table
    ref <- suppressWarnings(data.table::fread(ref_path))
    colnames(ref)[1] <- "EID"
    if (!is.null(keyword_query)) {
        rows <- grep(paste(keyword_query, collapse = "|"),
            data.table::transpose(ref),
            ignore.case = TRUE
        )
        ref <- ref[rows, ]
        messager(
            "+ ROADMAP::", formatC(nrow(ref),big.mark = ","),
            "annotation(s) identified that match:",
            paste(keyword_query,collapse = " | "),
            v=verbose
        )
    }
    #### Limit files ####
    if (!is.null(limit_files)) {
        messager("Limiting results to only",limit_files,"file(s).",v=verbose)
        ref <- ref[seq_len(limit_files), ]
    }
    return(ref)
}
