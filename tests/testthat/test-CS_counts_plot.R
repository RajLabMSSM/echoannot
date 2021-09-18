test_that("CS_counts_plot works", {
    gg_cs_counts <- echoannot::CS_counts_plot(
        merged_DT = echodata::Nalls2019_merged
    )
    testthat::expect_length(gg_cs_counts, 2)
    testthat::expect_true(methods::is(gg_cs_counts$plot, "gg"))
    testthat::expect_true(methods::is(gg_cs_counts$data, "data.frame"))
    testthat::expect_gte(nrow(gg_cs_counts$data), 300)
})
