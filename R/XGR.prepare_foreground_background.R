#' Prepare SNP sets for enrichment
#'
#' Prepare custom foreground and background SNPs sets for enrichment
#' tests with XGR annotations.
#'
#' @param subset_DT Data.frame with at least the following columns:
#' \describe{
#' \item{SNP}{SNP RSID}
#' \item{CHR}{chromosome}
#' \item{POS}{position}
#' }
#' @param foreground_filter Specify foreground by filtering SNPs
#' in \code{subset_DT}.
#' Write filter as a string (or \code{NULL} to include all SNPs).
#' @param background_filter Specify background by filtering SNPs
#' in \code{subset_DT}.
#' Write filter as a string (or \code{NULL} to include all SNPs).
#' @family XGR
#' @keywords internal
#' @examples
#' \dontrun{
#' fg_bg <- XGR.prepare_foreground_background(
#'     subset_DT = echodata::Nalls2019_merged,
#'     foreground_filter = "Consensus_SNP==TRUE",
#'     background_filter = "leadSNP==TRUE"
#' )
#' }
#' @importFrom dplyr %>% mutate select sample_n
XGR.prepare_foreground_background <- function(subset_DT,
                                              foreground_filter = "Support>0",
                                              background_filter = NULL,
                                              fg_sample_size = NULL,
                                              bg_sample_size = NULL,
                                              verbose = TRUE) {
    if (!exists("sampling_df")) sampling_df <- subset_DT
    messager("XGR:: Preparing foreground/background for enrichment test",
        v = verbose
    )
    #### Foreground ####
    fg <- subset(subset_DT, eval(parse(text = foreground_filter))) %>%
        dplyr::mutate(
            chrom = paste0(gsub("chr", "", CHR)),
            chromStart = POS,
            chromEnd = POS,
            name = SNP
        ) %>%
        dplyr::select(chrom, chromStart, chromEnd, name)

    #### Background ####
    if (any(is.na(background_filter))) {
        ## Optionally, can supply no background at all to XGR
        bg <- NULL
    } else {
        if (!is.null(background_filter)) {
            bg_DT <- subset(subset_DT, eval(parse(text = background_filter)))
        } else {
            bg_DT <- subset_DT
        }
        bg <- bg_DT %>%
            dplyr::mutate(
                chrom = paste0(gsub("chr", "", CHR)),
                chromStart = POS,
                chromEnd = POS,
                name = SNP
            ) %>%
            dplyr::select(chrom, chromStart, chromEnd, name)
    }


    #### Sample fg/bg (for bootstrapping) ####
    if (!is.null(fg_sample_size)) {
        messager("XGR:: Sampling", fg_sample_size, "foreground SNPs", v = verbose)
        fg <- fg %>% dplyr::sample_n(size = fg_sample_size)
    }
    if (!is.null(bg_sample_size)) {
        messager("XGR:: Sampling", bg_sample_size, "background SNPs", v = verbose)
        bg <- bg %>% dplyr::sample_n(size = bg_sample_size)
    }

    messager("XGR::", nrow(fg), "SNPs in foreground.")
    messager("XGR::", nrow(bg), "SNPs in background")
    return(list(
        "foreground" = fg,
        "background" = bg
    ))
}
