#' Gather Roadmap annotation metadata
#'
#' @param ref_path Where the ROADMAP metadata is stored.
#' @param keyword_query Search all columns in the Roadmap annotations metadata
#' and only query annotations that contain your keywords.
#' Can provide multiple keywords in list form:
#' \code{c("placenta","liver","monocytes")}
#'
#' @examples
#' ref <- ROADMAP_construct_reference(keyword_query = c(
#'     "placenta",
#'     "liver",
#'     "monocytes"
#' ))
#' @family ROADMAP
#' @export
#' @importFrom data.table transpose fread
ROADMAP_construct_reference <- function(ref_path =
                                            system.file(
                                                "extdata/ROADMAP",
                                                "ROADMAP_Epigenomic.js",
                                                package = "echoannot"
                                            ),
                                        keyword_query = NULL) {
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
            "+ ROADMAP::", nrow(ref),
            "annotation(s) identified that match `keyword_query`."
        )
    }
    return(ref)
}
