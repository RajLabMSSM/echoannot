test_that("ROADMAP.query_and_plot works", {
    
    grl.roadmap <- ROADMAP.query(
        gr.snp = echodata::BST1,
        keyword_query = "placenta")
    testthat::expect_length(grl.roadmap, 2)
    testthat::expect_true(methods::is(grl.roadmap[[1]],"GRanges"))
    testthat::expect_gte(length(grl.roadmap[[1]]), 400000)
})
