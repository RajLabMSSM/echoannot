test_that("XGR_plot works", {
    
    xgr_out <- echoannot::XGR_plot(dat = echodata::BST1[1:1000, ])
    testthat::expect_length(xgr_out, 2)
    testthat::expect_true(methods::is(xgr_out$data, "GRanges"))
    testthat::expect_true(methods::is(xgr_out$plot, "gg"))
})
