test_that("count_and_melt returns correct structure", {
    merged_annot <- data.table::data.table(
        Locus = rep("BST1", 6),
        Cell_type = c("neurons", "neurons", "microglia",
                       "microglia", "neurons", "microglia"),
        Assay = c("ATAC", "ATAC", "H3K27ac",
                   "H3K27ac", "H3K4me3", "ATAC"),
        SNP = c("rs1", "rs2", "rs3", "rs4", "rs5", "rs6"),
        Consensus_SNP = c(TRUE, TRUE, FALSE, TRUE, TRUE, FALSE)
    )
    result <- echoannot:::count_and_melt(
        merged_annot = merged_annot,
        snp_filter = "Consensus_SNP==TRUE",
        grouping_vars = c("Locus", "Cell_type", "Assay")
    )
    expect_true(data.table::is.data.table(result))
    expect_true("Count" %in% colnames(result))
    expect_true("SNP" %in% colnames(result))
    expect_true("Locus" %in% colnames(result))
})

test_that("count_and_melt with 2 grouping vars skips Celltype_Assay", {
    merged_annot <- data.table::data.table(
        Locus = rep("BST1", 4),
        Cell_type = c("neurons", "neurons", "microglia", "microglia"),
        SNP = c("rs1", "rs2", "rs3", "rs4"),
        Consensus_SNP = c(TRUE, FALSE, TRUE, FALSE)
    )
    result <- echoannot:::count_and_melt(
        merged_annot = merged_annot,
        snp_filter = "Consensus_SNP==TRUE",
        grouping_vars = c("Locus", "Cell_type")
    )
    expect_true(data.table::is.data.table(result))
    expect_true("Count" %in% colnames(result))
    ## With < 3 grouping vars, Celltype_Assay should not be created
    expect_false("Celltype_Assay" %in% colnames(result))
})
