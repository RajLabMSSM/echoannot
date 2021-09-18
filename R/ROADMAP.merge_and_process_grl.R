#' Standardize Roadmap query
#'
#' @param grl.roadmap Roadmap query results
#' @param n_top_tissues The number of top tissues to include,
#' sorted by greatest number of rows
#' (i.e. the number of genomic ranges within the window).
#' @family ROADMAP
#' @importFrom IRanges overlapsAny
#' @importFrom dplyr %>% group_by tally n_distinct
ROADMAP.merge_and_process_grl <- function(grl.roadmap,
                                          gr.snp,
                                          n_top_tissues = 5,
                                          sep = " ") {
    grl.roadmap.merged <- unlist(grl.roadmap)
    grl.roadmap.merged$Source <- names(grl.roadmap.merged)
    grl.roadmap.merged$Source <- gsub("_", sep, grl.roadmap.merged$Source)
    grl.roadmap.merged$ChromState <-
        lapply(
            grl.roadmap.merged$State,
            function(ROW) {
                base::strsplit(ROW, "_")[[1]][2]
            }
        ) %>% unlist()
    grl.roadmap.filt <- grl.roadmap.merged[unlist(lapply(
        grl.roadmap, function(e) {
            IRanges::overlapsAny(e, gr.snp, minoverlap = 1)
        }
    ))]
    if (!is.null(n_top_tissues)) {
        top_tissues <- data.frame(grl.roadmap.filt) %>%
            dplyr::group_by(Source) %>%
            dplyr::tally(sort = TRUE)
        grl.roadmap.filt <- subset(
            grl.roadmap.filt,
            Source %in% unique(top_tissues$Source[
                seq(1, min(
                    n_top_tissues,
                    dplyr::n_distinct(top_tissues$Source)
                ))
            ])
        )
    }
    return(grl.roadmap.filt)
}
