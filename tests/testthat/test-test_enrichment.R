test_that("test_enrichment works", {
    
    query_granges <- GenomicRanges::GRanges("chr6:165169213-167169213")
    
    ids1 <- "GSM2101439"
    #### genericPeaks: Without query_granges ####
    grlist1 <- echoannot::import_peaks(ids = ids1,
                                   builds = "hg19",
                                   query_granges = query_granges, 
                                   query_granges_build = "hg38") 
    
    
    #### narrowPeaks: With query_granges #### 
    #### broadPeaks: With query_granges #### 
    ids2 <- c("GSM945244","GSM1003455")
    grlist2 <- echoannot::import_peaks(ids = ids2,
                                   builds = "hg19",
                                   query_granges = query_granges, 
                                   query_granges_build = "hg38") 
    
    set.seed(2022)
    enrich <- test_enrichment(grlist1 = grlist1,
                              grlist2 = grlist2, 
                              genome = query_granges,
                              ntimes = 100)
    testthat::expect_true(all(enrich$gr1==ids1))
    testthat::expect_true(all(enrich$gr2==ids2))
    testthat::expect_equal(nrow(enrich), length(ids1)*length(ids2))
    
    
    # #### Nott data ####
    # nott_dat <- echoannot::NOTT2019_get_epigenomic_peaks(convert_to_granges = TRUE)
    # nott_gr <- GenomicRanges::split(x = nott_dat, 
    #                                 f = list(nott_dat$Cell_type)) 
    # query_granges2 <- regioneR::filterChromosomes(regioneR::getGenome(genome = "hg19"), 
    #                                               keep.chr = c("chr6","chr7","chr10"))
    # query_granges2 <- echodata::dt_to_granges(query_granges2, style = "NCBI")
    # enrich2 <- test_enrichment(grlist1 = nott_gr$astrocytes,
    #                            grlist2 = nott_gr$microglia, 
    #                            genome = query_granges2,
    #                            ntimes = 100)
    # testthat::expect_true(all(enrich$gr1==ids1))
    # testthat::expect_true(all(enrich$gr2==ids2))
    # testthat::expect_equal(nrow(enrich), length(ids1)*length(ids2))
})
