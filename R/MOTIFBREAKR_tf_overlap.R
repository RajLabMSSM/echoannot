MOTIFBREAKR_tf_overlap <- function(mb_merge,
                                   mb.encode,
                                   pvalue_threshold=1e-4){
    y <- data.frame(
        subset(mb.encode, 
               Refpvalue<pvalue_threshold | Altpvalue<pvalue_threshold)
    )
    dat_merged <- base::merge(
        x = subset(mb_merge, risk_pvalue<pvalue_threshold),
        y = y,
        by=c("SNP_id","geneSymbol"))
    if(all(dat_merged$risk_allele=="ALT")){
        dat_merged <- subset(dat_merged, Altpvalue.y<pvalue_threshold)
    } else {
        dat_merged <- subset(dat_merged, Refpvalue.y<pvalue_threshold)
    }
    
    TF_overlap <- unique(dat_merged$geneSymbol)
    return(TF_overlap)
}
