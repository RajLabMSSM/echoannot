test_that("CS_bin_plot works", {
    gg_cs_bin <- echoannot::CS_bin_plot(merged_DT = echodata::Nalls2019_merged)
    testthat::expect_length(gg_cs_bin, 2)
    testthat::expect_true(methods::is(gg_cs_bin$plot, "gg"))
    testthat::expect_true(methods::is(gg_cs_bin$data, "data.frame"))
    testthat::expect_gte(nrow(gg_cs_bin$data), 400)
})
