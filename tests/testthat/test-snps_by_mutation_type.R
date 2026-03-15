test_that("snps_by_mutation_type filters correctly", {
    merged_results <- data.table::data.table(
        Dataset = rep("test", 6),
        Gene = rep("GENE1", 6),
        SNP = c("rs1", "rs1", "rs2", "rs2", "rs3", "rs3"),
        consequence_type_tv = c(
            "missense_variant", "synonymous_variant",
            "missense_variant", "missense_variant",
            "synonymous_variant", "intron_variant"
        )
    )
    result <- echoannot:::snps_by_mutation_type(
        merged_results = merged_results,
        mutation_type = "missense_variant"
    )
    ## rs1 and rs2 have missense_variant, so should be in result
    expect_true("rs1" %in% result$SNP)
    expect_true("rs2" %in% result$SNP)
    ## rs3 does not have missense_variant
    expect_false("rs3" %in% result$SNP)
})

test_that("snps_by_mutation_type returns all consequence types for matching SNPs", {
    merged_results <- data.table::data.table(
        Dataset = rep("test", 4),
        Gene = rep("GENE1", 4),
        SNP = c("rs1", "rs1", "rs2", "rs2"),
        consequence_type_tv = c(
            "missense_variant", "synonymous_variant",
            "intron_variant", "intron_variant"
        )
    )
    result <- echoannot:::snps_by_mutation_type(
        merged_results = merged_results,
        mutation_type = "missense_variant"
    )
    ## rs1 should show both consequence types
    rs1_results <- result[result$SNP == "rs1", ]
    expect_true(nrow(rs1_results) >= 1)
})

test_that("snps_by_mutation_type returns empty for no matches", {
    merged_results <- data.table::data.table(
        Dataset = "test",
        Gene = "GENE1",
        SNP = "rs1",
        consequence_type_tv = "synonymous_variant"
    )
    result <- echoannot:::snps_by_mutation_type(
        merged_results = merged_results,
        mutation_type = "missense_variant"
    )
    expect_equal(nrow(result), 0)
})

test_that("snps_by_mutation_type with custom mutation_type", {
    merged_results <- data.table::data.table(
        Dataset = rep("test", 3),
        Gene = rep("GENE1", 3),
        SNP = c("rs1", "rs2", "rs3"),
        consequence_type_tv = c(
            "intron_variant", "intron_variant", "synonymous_variant"
        )
    )
    result <- echoannot:::snps_by_mutation_type(
        merged_results = merged_results,
        mutation_type = "intron_variant"
    )
    expect_true("rs1" %in% result$SNP)
    expect_true("rs2" %in% result$SNP)
    expect_false("rs3" %in% result$SNP)
})
