count_and_melt <- function(merged_annot,
                           snp_filter = "Consensus_SNP==TRUE",
                           grouping_vars = c("Locus", "Cell_type", "Assay")) {
    . <- SNP <- NULL

    consensus_melt <-
        data.table::setDT(
            merged_annot
        )[, .(
            Count = dplyr::n_distinct(SNP[eval(parse(text = snp_filter))],
                na.rm = TRUE
            )
        ),
        by = grouping_vars
        ]
    if (length(grouping_vars) >= 3) {
        consensus_melt <- subset(consensus_melt,
            !is.na(dplyr::vars(grouping_vars[2])) &
                !is.na(dplyr::vars(grouping_vars[3])),
            .drop = FALSE
        ) %>%
            dplyr::mutate(Celltype_Assay = paste0(
                eval(parse(text = grouping_vars[2])), "_",
                eval(parse(text = grouping_vars[3]))
            ))
    }
    return(consensus_melt)
}
