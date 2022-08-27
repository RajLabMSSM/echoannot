#' Summarize \pkg{motifbreakR} + \pkg{echolocatoR} results
#' 
#' Summarize \pkg{motifbreakR} + \pkg{echolocatoR} results after they have 
#' been merged together with \link[echoannot]{MOTIFBREAKR_filter}.
#' @param mb_merge Output of \link[echoannot]{MOTIFBREAKR_filter}.
#' @returns Per-locus summary in data.frame format.
#' @family motifbreakR
#' 
#' @export
#' @importFrom dplyr group_by tally arrange desc summarise mutate n_distinct
#' @importFrom data.table data.table
#' @examples 
#' merged_DT <- echodata::get_Nalls2019_merged()
#' mb_res <- MOTIFBREAKR(rsid_list = c("rs11175620"),
#'                       # limit the number of datasets tests 
#'                       # for demonstration purposes only
#'                       pwmList_max = 4,
#'                       calculate_pvals = FALSE)
#' mb_merge <- MOTIFBREAKR_filter(mb_res = mb_res,
#'                                merged_DT = merged_DT,
#'                                pvalue_threshold = NULL)
#' summary_ls <- MOTIFBREAKR_summarize(mb_merge = mb_merge)                         
MOTIFBREAKR_summarize <- function(mb_merge){ 
    effect <- dataSource <- Locus <-alleleDiff <- top_disrupting_SNP <-
        seqMatch <- SNP <- Consensus_SNP <- geneSymbol <- 
        leadSNP <- Support <- NULL;
    # Tally hits hits per
    db_tally <- mb_merge |>
        dplyr::group_by(effect, dataSource) |>
        dplyr::tally() |>
        dplyr::arrange(effect,dplyr::desc(n)) |>
        data.table::data.table()  
    top_snps <- mb_merge |>
        dplyr::group_by(Locus) |>
        # dplyr::arrange(desc(risk_score), risk_pct) |>
        dplyr::arrange(dplyr::desc(alleleDiff)) |>
        dplyr::slice(1)
    top_rsids <- unique(top_snps$SNP) 
    
    locus_tally <- mb_merge |>
        dplyr::group_by(Locus) |>
        dplyr::summarise(
                         ## These cols aren't useful
                         ## unless you have the full fine-mapping data
                         ## which is filtered in prior steps.
                         # n_lead=
                         #     dplyr::n_distinct(SNP[leadSNP]),
                         # n_UCS=
                         #     dplyr::n_distinct(SNP[Support>0]),
                         # n_consensus=
                         #     dplyr::n_distinct(SNP[Consensus_SNP]),
                         # lead_in_consensus=
                         #     SNP[leadSNP] %in% SNP[Consensus_SNP],
                         top_disrupting_SNP=
                             paste(unique(SNP[SNP%in%top_rsids]),
                                   collapse = '; '),
                         top_TF=
                             paste(unique(geneSymbol[SNP%in%top_rsids]),
                                   collapse='; '),
                         top_sequence=
                             paste(unique(gsub(" ","",
                                               seqMatch[SNP%in%top_rsids])),
                                   collapse='; '),  
                         .groups = "keep"
        ) |>
        dplyr::mutate(
            top_disrupting_SNP_is_lead=
                top_disrupting_SNP %in% unique(subset(mb_merge,
                                                      leadSNP)$SNP),
            top_disrupting_SNP_in_UCS=
                top_disrupting_SNP %in% unique(subset(mb_merge, 
                                                      Support>0)$SNP),
            top_disrupting_SNP_in_consensus=
                top_disrupting_SNP %in% unique(subset(mb_merge,
                                                      Consensus_SNP)$SNP)
        ) |>  
        unique() |> 
        data.table::data.table()
    return(list(db_tally=db_tally, 
                locus_tally=locus_tally))
}
