test_that("import_bedgraph_chroms works", {
    
    gsm <- "GSM4703766"
    chroms <- "chr6"
    
    #### Get links to supplementary files on GEO ####
    links <- echoannot:::get_geo_supplementary_files(gsm = gsm) 
    #### Import bedgraph subset: chr6 ####
    gr <- echoannot:::import_bedgraph_chroms(URL = links$bedGraph, 
                                             chroms = chroms)
    testthat::expect_true(methods::is(gr,"GRanges"))
    testthat::expect_length(gr, 79829)
    testthat::expect_equal(as.character(unique(GenomicRanges::seqnames(gr))),
                           chroms)
    
    ### Failing because bigwig files cannot be read by rtracklayer ####
    # gsm <- "GSM5684359"
    # #### Get links to supplementary files on GEO ####
    # links <- echoannot:::get_geo_supplementary_files(gsm = gsm) 
    # #### Import bigwig subset: chr6 ####
    # gr <- echoannot:::import_bedgraph_chroms(URL = links$bigWig, 
    #                                          chroms = chroms,
    #                                          import_format = "BigWig")
    # testthat::expect_true(methods::is(gr,"GRanges"))
    # testthat::expect_length(gr, 79829)
    # testthat::expect_equal(as.character(unique(GenomicRanges::seqnames(gr))),
    #                        chroms)
})
