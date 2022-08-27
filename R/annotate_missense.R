#' Annotate any missense variants
#'
#' Annotate any missense variants with \link[biomaRt]{getBM}.
#' @param merged_DT Merged fine-mapping results data from
#'  \link[echolocatoR]{finemap_loci}.
#' @param snp_filter Row-wise filter to apply to \code{merged_DT} filter 
#' (provided as a string). 
#' 
#' @family annotate
#' @export
#' @importFrom dplyr group_by summarise
#' @importFrom data.table data.table merge.data.table
#' @examples
#' \dontrun{
#' merged_DT <- echodata::get_Nalls2019_merged()
#' annotated_DT <- annotate_missense(merged_DT = merged_DT)
#' }
annotate_missense <- function(merged_DT,
                              snp_filter = "Support>0") {
    
    refsnp_id <- Support <- Consensus_SNP <- consequence_type_tv <- NULL 
    snp_info <- biomart_snp_info(
        snp_list = unique(subset(merged_DT, 
                                 eval(parse(text = snp_filter)))$SNP)
    )
    # unique(snp_info$consequence_type_tv)
    missense <- suppressMessages(
        snp_info |>
            dplyr::group_by(refsnp_id) |>
            dplyr::summarise(Missense = ifelse(
                any(consequence_type_tv == "missense_variant",
                    na.rm = TRUE
                ), TRUE, FALSE
            )) |>
            data.table::data.table()
    )
    merged_DT <- data.table::merge.data.table(merged_DT, missense,
        all.x = TRUE,
        by.x = "SNP",
        by.y = "refsnp_id"
    )
    # missense_counts <- suppressMessages(merged_DT |> 
    #                                         dplyr::group_by(Locus) |>
    #   dplyr::summarise(Missense=sum(Missense, na.rm=TRUE)))
    messager(
        sum(subset(merged_DT, Support > 0)$Missense, na.rm = TRUE),
        "missense mutations detected in UCS."
    )
    messager(
        sum(subset(merged_DT, Consensus_SNP)$Missense, na.rm = TRUE),
        "missense mutations detected in Consensus SNPs"
    )
    return(merged_DT)
}
