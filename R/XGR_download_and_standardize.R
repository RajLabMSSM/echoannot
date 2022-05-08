#' Download, standardize, and merge XGR annotations
#'
#' Merges a list of XGR annotations into a single GRanges object
#'
#' @param lib.selections Which XGR annotations to check overlap with.
#' For full list of libraries see
#' \href{http://XGR_r-forge.r-project.org/#annotations-at-the-genomic-region-level}{
#'  here.}
#'  Passed to the \code{RData.customised} argument in \link[XGR]{xRDataLoader}.
#' @param as_grangesList Return as a \code{GRangesList},
#'  instead of a single merged \code{GRanges} object.
#' @param dat Fine-mapping results.
#' @param nThread Number of cores to parallelise across.
#'
#' @return GRangesList
#' @family XGR
#' @examples
#' gr.lib <- echoannot::XGR_download_and_standardize(
#'     lib.selections = c("ENCODE_DNaseI_ClusteredV3_CellTypes"),
#'     dat = echodata::BST1
#' )
#' @export
#' @importFrom parallel mclapply
#' @importFrom XGR xRDataLoader
#' @importFrom dplyr %>%
#' @importFrom GenomicRanges GRangesList
#' @importFrom methods is
XGR_download_and_standardize <- function(lib.selections = c(
                                             "ENCODE_TFBS_ClusteredV3_CellTypes",
                                             "TFBS_Conserved",
                                             "Uniform_TFBS"
                                         ),
                                         as_grangesList = FALSE,
                                         dat,
                                         nThread = 1) {
    # Iterate over XGR libraries
    gr.lib <- lapply(lib.selections, function(lib.name) {
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
                }) %>% unlist()
            } else {
                grl$name <- names(grl)
                return(grl)
            }
        }, mc.cores = nThread) %>% unlist() # return all_GRL
        # Rename GRanges after they've been unnested
        names(all_GRL) <- names(unlist(GR.annotations))

        # Merge lists together
        if (!is.null(all_GRL)) {
            ALL_GRL <- unlist(GenomicRanges::GRangesList(all_GRL))
            ALL_GRL <- granges_overlap(
                dat1 = dat,
                chrom_col.1 = "CHR",
                start_col.1 = "POS",
                end_col.1 = "POS",
                dat2 = ALL_GRL
            )
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
    # Merge
    gr.lib <- GenomicRanges::GRangesList(unlist(gr.lib))
    if (as_grangesList == FALSE) {
        gr.lib <- unlist(gr.lib)
    }
    return(gr.lib)
}
