test_that("merge_finemapping_results works", {
    local_files <- echodata::portal_query(
        phenotypes = "parkinson",
        LD_panels = "UKB",
        loci = c("BST1", "LRRK2"),
        file_types = "multi_finemap"
    )
    dataset <- dirname(dirname(dirname(local_files)))[1]
    testthat::expect_true(dir.exists(dataset))

    merged_DT <- echoannot::merge_finemapping_results(
        dataset = dataset,
        minimum_support = 1,
        haploreg_annotation = TRUE
    )

    testthat::expect_true("Promoter_histone_marks" %in% colnames(merged_DT))
    testthat::expect_gte(nrow(merged_DT), 10)
    testthat::expect_true(methods::is(merged_DT, "data.table"))
    testthat::expect_equal(dplyr::n_distinct(merged_DT$Locus), 2)
})
