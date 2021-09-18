test_that("ROADMAP.query_and_plot works", {
 
    roadmap_plot_query <- ROADMAP.query_and_plot(subset_DT = echodata::BST1,
                                                 keyword_query = "monocytes")
    testthat::expect_length(roadmap_plot_query, 2)
    testthat::expect_true(methods::is(roadmap_plot_query$Roadmap_plot, "gg"))
    testthat::expect_true(methods::is(roadmap_plot_query$Roadmap_query, "GRanges"))
    testthat::expect_gte(length(roadmap_plot_query$Roadmap_query), 700)
})
