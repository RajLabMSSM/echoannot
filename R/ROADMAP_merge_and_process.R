#' Standardize Roadmap query
#' 
#' Standardize and filter Roadmap query results.
#' @param grl.roadmap Roadmap query results.
#' @inheritParams ROADMAP_query
#' @inheritParams IRanges::subsetByOverlaps
#' @family ROADMAP
#' 
#' @keywords internal
#' @importFrom echodata dt_to_granges
#' @importFrom IRanges subsetByOverlaps
#' @importFrom dplyr group_by tally n_distinct
ROADMAP_merge_and_process <- function(grl.roadmap,
                                      gr.snp,
                                      n_top = NULL,
                                      minoverlap=1, 
                                      verbose = TRUE) {
    
    Source <- NULL;
    messager("Merging and processing ROADMAP annotations.", 
             v = verbose)
    gr.snp <- echodata::dt_to_granges(dat = gr.snp,
                                      style = "UCSC",
                                      verbose = FALSE)
    grl.ROADMAP_merged <- unlist(grl.roadmap)
    #### Filter by overlap ####
    grl.roadmap.filt <- IRanges::subsetByOverlaps(x = grl.ROADMAP_merged, 
                                                  ranges = gr.snp, 
                                                  minoverlap = minoverlap) 
    if (!is.null(n_top)) {
        top_tissues <- data.frame(grl.roadmap.filt) |>
            dplyr::group_by(Source) |>
            dplyr::tally(sort = TRUE)
        grl.roadmap.filt <- subset(
            grl.roadmap.filt,
            Source %in% unique(top_tissues$Source[
                seq_len(min(
                    n_top,
                    dplyr::n_distinct(top_tissues$Source)
                ))
            ])
        )
    }
    return(grl.roadmap.filt)
}
