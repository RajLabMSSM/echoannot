#' Download, standardize, and merge XGR annotations
#'
#' Merges a list of XGR annotations into a single
#'  \link[GenomicRanges]{GRangesList} 
#'  (or merged \link[GenomicRanges]{GRanges}) object.
#' @param lib.selections Which XGR annotations to check overlap with.
#' For full list of libraries see
#' \href{http://XGR_r-forge.r-project.org/#annotations-at-the-genomic-region-level}{
#'  here.}
#'  Passed to the \code{RData.customised} argument in \link[XGR]{xRDataLoader}.
#' @param n_top Filter to only the top N annotations 
#' that have the greatest amount of overlap with the genomic coordinates of
#' \code{dat}. 
#' @param as_grangesList Return as a \link[GenomicRanges]{GRangesList},
#'  instead of a single merged \link[GenomicRanges]{GRanges} object.
#' @param dat data.table of genomic coordinates to query with.
#' Set as \code{NULL} to return genome-wide data.
#' @param nThread Number of cores to parallelise across.
#' @returns \link[GenomicRanges]{GRangesList}
#' @family XGR
#' 
#' @export
#' @importFrom parallel mclapply
#' @importFrom XGR xRDataLoader
#' @importFrom GenomicRanges GRangesList 
#' @examples
#' gr.lib <- echoannot::XGR_query(
#'     lib.selections = c("ENCODE_DNaseI_ClusteredV3_CellTypes"),
#'     dat = echodata::BST1, 
#'     n_top = 1)
XGR_query <- function(lib.selections = c("ENCODE_TFBS_ClusteredV3_CellTypes",
                                         "TFBS_Conserved",
                                         "Uniform_TFBS"
                                         ), 
                                         as_grangesList = FALSE,
                                         dat=NULL,
                                         n_top=NULL,
                                         nThread=1) {
    # Iterate over XGR libraries
    gr.lib <- lapply(lib.selections, 
                     function(lib.name) {
        GR.annotations <- XGR::xRDataLoader(RData.customised = lib.name)
        if (is.null(GR.annotations)) {
            stop("XGR library name not recognized: ", lib.name)
        }
        # Iterate over lists within each library
        all_GRL <- parallel::mclapply(names(GR.annotations), function(n1) {
            grl <- GR.annotations[[n1]]

            #### Handle both nested and unnested entries ####
            if (is.list(grl)) {
                # grl$name <- names(unlist(grl))
                GRL <- lapply(names(grl), function(n2) {
                    gr <- grl[[n2]]
                    gr$source <- n1
                    gr$assay <- n2
                    return(gr)
                }) |> unlist()
            } else {
                grl$name <- names(grl)
                return(grl)
            }
        }, mc.cores = nThread) |> unlist() # return all_GRL
        # Rename GRanges after they've been unnested
        names(all_GRL) <- names(unlist(GR.annotations))

        # Merge lists together
        if (!is.null(all_GRL)) {
            ALL_GRL <- unlist(GenomicRanges::GRangesList(all_GRL))
            if(!is.null(dat)){
                ALL_GRL <- granges_overlap(
                    dat1 = dat,
                    chrom_col.1 = "CHR",
                    start_col.1 = "POS",
                    end_col.1 = "POS",
                    dat2 = ALL_GRL
                )
            }
            # Parse metadata
            ALL_GRL$library <- lib.name
            ALL_GRL$fullname <- names(ALL_GRL)
            ALL_GRL <- XGR_parse_metadata(
                gr.lib = ALL_GRL,
                lib.name = lib.name
            )
            return(ALL_GRL)
        } else {
            return(NULL)
        }
    }) # return OVERLAP
    #### Merge ####
    gr.lib <- GenomicRanges::GRangesList(unlist(gr.lib))
    if (isFALSE(as_grangesList)) {
        gr.lib <- unlist(gr.lib)
    }
    #### Filter annot ####
    gr.lib <- XGR_filter_sources(
        gr.lib = gr.lib,
        n_top_sources = n_top)
    gr.lib <- XGR_filter_assays(
        gr.lib = gr.lib,
        n_top_assays = n_top)
    #### Return ####
    return(gr.lib)
}
