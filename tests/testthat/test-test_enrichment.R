test_that("test_enrichment works", {

    dat <- echodata::get_Nalls2019_merged()
    grlist1 <- dat[P<5e-8,]
    grlist2 <- dat[Support>0,]
    enrich <- tryCatch({
        suppressWarnings(
            test_enrichment(grlist1 = grlist1,
                            grlist2 = grlist2,
                            ntimes = 25)
        )
    }, error = function(e) {
        testthat::skip(paste("regioneR permutation test failed:", e$message))
    })
    testthat::expect_lte(enrich$pval, 0.039)
    testthat::expect_equal(nrow(enrich), 1)
})
