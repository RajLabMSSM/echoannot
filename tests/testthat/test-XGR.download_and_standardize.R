test_that("XGR.download_and_standardize works", {
    
    gr.lib <- XGR.download_and_standardize(
        lib.selections = c("ENCODE_DNaseI_ClusteredV3_CellTypes"),
        finemap_dat = echodata::BST1)
    testthat::expect_true(methods::is(gr.lib,"GRanges"))
    testthat::expect_gte(length(gr.lib), 8000)
})
