test_that("import_peaks works", {
    
    #### setup ####
    query_granges <- GenomicRanges::GRanges("chr6:165169213-167169213")
    
    
    #### genericPeaks: Without query_granges ####
    ids <- "GSM2101439" 
    grl <- echoannot::import_peaks(ids = ids,
                                   builds = "hg19")
    testthat::expect_true(names(grl)==ids)
    testthat::expect_true(methods::is(grl[[1]], "GRanges"))
    testthat::expect_length(grl[[1]], 96376)
    remove(grl)
    
    #### genericPeaks: With query_granges #### 
    ids <- "GSM2101439" 
    grl <- echoannot::import_peaks(ids = ids,
                                   builds = "hg19",
                                   query_granges = query_granges, 
                                   query_granges_build = "hg38")
    testthat::expect_true(names(grl)==ids)
    testthat::expect_true(methods::is(grl[[1]], "GRanges"))
    testthat::expect_length(grl[[1]], 68)
    remove(grl)
    
    
    #### broadPeaks: With query_granges #### 
    ids <- "GSM1003455"
    grl <- echoannot::import_peaks(ids = ids,
                                   builds = "hg19",
                                   query_granges = query_granges, 
                                   query_granges_build = "hg38")
    testthat::expect_true(names(grl)==ids)
    testthat::expect_true(methods::is(grl[[1]], "GRanges"))
    testthat::expect_length(grl[[1]], 2)
    remove(grl)
    
    
    #### narrowPeaks: With query_granges #### 
    ids <- "GSM945244"
    grl <- echoannot::import_peaks(ids = ids,
                                   builds = "hg19",
                                   query_granges = query_granges, 
                                   query_granges_build = "hg38")
    testthat::expect_true(names(grl)==ids)
    testthat::expect_true(methods::is(grl[[1]], "GRanges"))
    testthat::expect_length(grl[[1]], 98)
    remove(grl)
    
    
    #### called peaks: Without query_granges #### 
    ids <- "GSM4703766"
    grl <- echoannot::import_peaks(ids = ids,
                                   builds = "hg19")
    testthat::expect_true(names(grl)==ids)
    testthat::expect_true(methods::is(grl[[1]], "GRanges"))
    testthat::expect_length(grl[[1]], 18112)
    remove(grl)
    
    
     #### called peaks: With query_granges #### 
    ids <- "GSM4703766"
    grl <- echoannot::import_peaks(ids = ids,
                                   builds = "hg19",
                                   query_granges = query_granges, 
                                   query_granges_build = "hg38")
    testthat::expect_true(names(grl)==ids)
    testthat::expect_true(methods::is(grl[[1]], "GRanges"))
    testthat::expect_length(grl[[1]], 15)
    remove(grl)
    
    
    #### called peaks from bigWig: With query_granges #### 
    ## broken until further notice:
    ## https://github.com/lawremi/rtracklayer/issues/62
    # ids <- "GSM5684359"
    # grl <- echoannot::import_peaks(ids = ids,
    #                                builds = "hg38",
    #                                query_granges = query_granges, 
    #                                query_granges_build = "hg38")
    # testthat::expect_true(names(grl)==ids)
    # testthat::expect_true(methods::is(grl[[1]], "GRanges"))
    # testthat::expect_length(grl[[1]], 15)
    # remove(grl)
})
