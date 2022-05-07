test_that("call_peaks works", {
    
    
    gsm <- "GSM4703766" 
    links <- echoannot:::get_geo_supplementary_files(gsm = gsm) 
    query_granges <- GenomicRanges::GRanges("chr6:165169213-167169213")
    gr <- rtracklayer::import(con = links$bedGraph, which = query_granges)
    
    #### From bedGraph #### 
    tmp <- tempfile(fileext = ".bedgraph")
    rtracklayer::export.bedGraph(object = gr, con = tmp)
    peaks1 <- echoannot::call_peaks(bedgraph_path = tmp)
    ## Tests
    testthat::expect_true(methods::is(peaks1,"GRanges"))
    testthat::expect_length(peaks1, 11)
    
    #### From bigWig #### 
    tmp <- tempfile(fileext = ".bigwig") 
    gr <- echoannot:::fix_seqinfo(gr = gr, build = "hg19")
    rtracklayer::export.bw(object = gr, con = tmp)
    peaks2 <- echoannot::call_peaks(bedgraph_path = tmp)
    ## Tests
    testthat::expect_true(methods::is(peaks2,"GRanges"))
    testthat::expect_length(peaks2, 11)
    
    #### Check that peaks from bedGraph vs. bigWig are identical ####
    testthat::expect_equal(peaks1, peaks2)
})
