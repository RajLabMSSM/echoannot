test_that("ROADMAP_plot works", {
    
    dat <- echodata::BST1[1:1000, ]
    
    roadmap_out <- echoannot::ROADMAP_plot(
        dat = dat,
        roadmap_query = "placenta"
    )
    testthat::expect_length(roadmap_out, 2)
    testthat::expect_true(methods::is(roadmap_out$data, "GRanges"))
    testthat::expect_true(methods::is(roadmap_out$plot, "gg"))
})
