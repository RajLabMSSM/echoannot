test_that("import_peaks works", {
    
    #### setup ####
    query_granges <- GenomicRanges::GRanges("chr6:165169213-167169213")
    

    #### genericPeak: Without query_granges ####
    ids <- "GSM2101439" 
    grl <- echoannot::import_peaks(ids = ids,
                                   builds = "hg19",
                                   regex_queries = list(genericPeak="peak"))
    testthat::expect_true(names(grl)==ids)
    testthat::expect_true(methods::is(grl[[1]], "GRanges"))
    testthat::expect_length(grl[[1]], 96376)
    remove(grl)
    
    #### genericPeak: With query_granges #### 
    ids <- "GSM2101439" 
    grl <- echoannot::import_peaks(ids = ids,
                                   builds = "hg19",
                                   query_granges = query_granges, 
                                   query_granges_build = "hg38",
                                   regex_queries = list(genericPeak="peak"))
    testthat::expect_true(names(grl)==ids)
    testthat::expect_true(methods::is(grl[[1]], "GRanges"))
    testthat::expect_length(grl[[1]], 68)
    remove(grl)
    
    
    #### broadPeak: With query_granges #### 
    ids <- "GSM1003455"
    grl <- echoannot::import_peaks(ids = ids,
                                   builds = "hg19",
                                   query_granges = query_granges, 
                                   query_granges_build = "hg38",
                                   regex_queries = list(broadPeak="broadpeak"))
    testthat::expect_true(names(grl)==ids)
    testthat::expect_true(methods::is(grl[[1]], "GRanges"))
    testthat::expect_length(grl[[1]], 3)
    remove(grl)
    
    
    #### narrowPeak: With query_granges #### 
    ids <- "GSM945244"
    grl <- echoannot::import_peaks(ids = ids,
                                   builds = "hg19",
                                   query_granges = query_granges, 
                                   query_granges_build = "hg38",
                                   regex_queries = list(narrowPeak="narrowpeak"))
    testthat::expect_true(names(grl)==ids)
    testthat::expect_true(methods::is(grl[[1]], "GRanges"))
    testthat::expect_length(grl[[1]], 115)
    remove(grl)
    
    
    #### called peaks: Without query_granges #### 
    ids <- "GSM4703766"
    grl <- echoannot::import_peaks(ids = ids,
                                   builds = "hg19", 
                                   regex_queries = list(bedGraph="bedgraph|graph.gz|bdg.gz",
                                                        bigWig="bigwig|bw$"))
    testthat::expect_true(names(grl)==ids)
    testthat::expect_true(methods::is(grl[[1]], "GRanges"))
    testthat::expect_length(grl[[1]], 18112)
    remove(grl)
    
    
     #### called peaks: With query_granges #### 
    ids <- "GSM4703766"
    grl <- echoannot::import_peaks(ids = ids,
                                   builds = "hg19",
                                   query_granges = query_granges, 
                                   query_granges_build = "hg38",
                                   regex_queries = list(bedGraph="bedgraph|graph.gz|bdg.gz",
                                                        bigWig="bigwig|bw$"))
    testthat::expect_true(names(grl)==ids)
    testthat::expect_true(methods::is(grl[[1]], "GRanges"))
    testthat::expect_length(grl[[1]], 15)
    remove(grl)
    
    
    #### called peaks from bigWig: With query_granges ####  
    ids <- "GSM5684359"
    query_granges <- regioneR::filterChromosomes(
        regioneR::getGenome("hg38"), 
        keep.chr = "chr4"
    )
    grl <- echoannot::import_peaks(ids = ids,
                                   builds = "hg38",
                                   query_granges = query_granges,
                                   query_granges_build = "hg38",
                                   regex_queries = list(bedGraph="bedgraph|graph.gz|bdg.gz",
                                                        bigWig="bigwig|bw$"))
    testthat::expect_true(names(grl)==ids)
    testthat::expect_true(methods::is(grl[[1]], "GRanges"))
    testthat::expect_length(grl[[1]], 4)
    remove(grl)
})
