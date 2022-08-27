MOTIFBREAKR_summarize <- function(mb_merge){ 
    # Tally hits hits per
    db_tally <- mb_merge |>
        dplyr::group_by(effect, dataSource) |>
        dplyr::tally() |>
        dplyr::arrange(effect,dplyr::desc(n)) |>
        data.frame()
    print(db_tally)
    
    top_snps <- mb_merge |>
        dplyr::group_by(Locus) |>
        # dplyr::arrange(desc(risk_score), risk_pct) |>
        dplyr::arrange(desc(alleleDiff)) |>
        dplyr::slice(1)
    top_rsids <- unique(top_snps$SNP)
    
    locus_tally <- mb_merge |>
        dplyr::group_by(Locus) |>
        dplyr::summarise(n_lead=dplyr::n_distinct(SNP[leadSNP]),
                         n_UCS=dplyr::n_distinct(SNP[Support>0]),
                         n_consensus=dplyr::n_distinct(SNP[Consensus_SNP]),
                         lead_in_consensus=SNP[leadSNP] %in% SNP[Consensus_SNP],
                         # consensus_SNPs=paste(unique(SNP[Consensus_SNP]), collapse = ", "),
                         top_disrupting_SNP=paste(unique(SNP[SNP%in%top_rsids]), collapse = '; '),
                         top_TF=paste(unique(geneSymbol[SNP%in%top_rsids]), collapse='; '),
                         top_sequence=paste(unique(gsub(" ","",seqMatch[SNP%in%top_rsids])), collapse='; '),
                         # consensus_RefAlt=paste(unique(.[Consensus_SNP,c("SNP","risk_allele")])$risk_allele, collapse=",")
                         # all_disrupting_SNPs=paste(unique(SNP), collapse = '; '),
                         # all_TFs=paste(unique(geneSymbol), collapse='; '),
                         # all_sequences=paste(unique(gsub(" ","",seqMatch)), collapse='; ')
        ) |>
        dplyr::mutate(top_disrupting_SNP_is_lead=top_disrupting_SNP %in% unique(subset(mb_merge, leadSNP)$SNP),
                      top_disrupting_SNP_in_UCS=top_disrupting_SNP %in% unique(subset(mb_merge, Support>0)$SNP),
                      top_disrupting_SNP_in_consensus=top_disrupting_SNP %in% unique(subset(mb_merge, Consensus_SNP)$SNP)
        ) |>
        data.frame() |> unique()
    
    # if(save_path!=FALSE){
    #   data.table::fwrite(locus_tally, "~/Desktop/Fine_Mapping/Data/GWAS/Nalls23andMe_2019/_genome_wide/motifbreakR/motifbreakR_locus_tally_ALL.csv", sep=",")
    # }
    return(locus_tally)
}
