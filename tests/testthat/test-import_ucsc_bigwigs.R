test_that("import_ucsc_bigwigs works", {
  
    
    bigwig_metadata <- echoannot::NOTT2019_bigwig_metadata[1,]
    query_dat = echodata::BST1

    bw.gr <- echoannot::import_ucsc_bigwigs(query_dat = query_dat,
                                            bigwig_metadata = bigwig_metadata)
    testthat::expect_true(methods::is(bw.gr,"GRanges"))
    testthat::expect_gte(length(bw.gr), 32000)
    testthat::expect_true(all.equal(colnames(bw.gr@elementMetadata),
                                    c("score","Cell_type",
                                      "Assay","Experiment")))
})
