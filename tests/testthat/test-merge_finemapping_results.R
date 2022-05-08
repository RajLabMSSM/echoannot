test_that("merge_finemapping_results works", {
    
    files <- echodata::get_Nalls2019_loci()
    dataset <- dirname(files[[1]])
    testthat::expect_true(dir.exists(dataset))

    merged_DT <- echoannot:: merge_finemapping_results(
        dataset = dataset,
        minimum_support = 1,
        haploreg_annotation = TRUE
    )
    testthat::expect_true("Promoter_histone_marks" %in% colnames(merged_DT))
    testthat::expect_gte(nrow(merged_DT), 10)
    testthat::expect_true(methods::is(merged_DT, "data.table"))
    testthat::expect_equal(dplyr::n_distinct(merged_DT$Locus), 3)
})
