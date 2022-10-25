#' Filter chromatin states
#' 
#' Annotate and filter a \link[GenomicRanges]{GRangesList} using
#'  descriptions of chromatin states from ROADMAP metadata.
#' @param grl A \link[GenomicRanges]{GRangesList}.
#' @param chrom_states Chromatin states to keep. If \code{NULL} (default),
#' data will not be filtered at all (only annotated).
#' @param chrom_states_col Column name in which chromatin state info is stored.
#' @param verbose Print messages.
#' @returns Annotated/filtered \link[GenomicRanges]{GRangesList}.
#' 
#' @keywords internal 
#' @importFrom BiocGenerics %in% 
#' @importFrom GenomicRanges mcols
#' @importFrom stringr str_split
filter_chromatin_states <- function(grl,
                                    chrom_states = NULL, # c("Quies")
                                    chrom_states_col = "name",
                                    verbose = TRUE){
    
    messager("Annotating chromatin states.",v=verbose)
    if(echodata::is_granges(grl)){
        grl <- list(grl) 
    }
    ckey <- ROADMAP_chromatin_states(as_dict = TRUE)
    
    grl <- lapply(grl, 
                  function(gr){
        GenomicRanges::mcols(gr)$chrom_type <- 
            stringr::str_split(GenomicRanges::mcols(gr)[[chrom_states_col]],
                               "_",n = 2, simplify = TRUE)[,2]
        GenomicRanges::mcols(gr)$chrom_state <- ckey[gr$chrom_type]
        if(!is.null(chrom_states)){
            messager("Using only selected chromatin states:",
                     paste(chrom_states,collapse = ","),v=verbose)
            gr <- gr[tolower(gr$chrom_type) %in% tolower(chrom_states)]
        }
        return(gr)
    })
    grl <- GenomicRanges::GRangesList(grl)
    return(grl)
}
