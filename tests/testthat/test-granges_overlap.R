test_that("granges_overlap works", {
 
    dat1 <- echodata::BST1
    dat2 <- echoannot::xgr_query
    GenomicRanges::mcols(dat2) <- NULL

    gr.hits <- echoannot::granges_overlap(dat1 = dat1,
                                          dat2 = dat2,
                                          chrom_col.1 = "CHR",
                                          start_col.1 = "POS")
    testthat::expect_true(methods::is(gr.hits,"GRanges"))
    testthat::expect_true(length(gr.hits) > 0)
    testthat::expect_true(length(gr.hits) < nrow(dat1) )
    testthat::expect_true(length(gr.hits) < length(dat2) )
})
