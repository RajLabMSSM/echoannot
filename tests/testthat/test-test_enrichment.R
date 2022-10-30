test_that("test_enrichment works", {
    
    dat <- echodata::get_Nalls2019_merged() 
    grlist1 <- dat[P<5e-8,]
    grlist2 <- dat[Support>0,] 
    testthat::expect_warning(
        enrich <- test_enrichment(grlist1 = grlist1,
                                  grlist2 = grlist2,  
                                  ntimes = 25)
    )
    testthat::expect_lte(enrich$pval, 0.039)
    testthat::expect_equal(nrow(enrich), 1) 
    
    # #### Nott data ####
    # nott_dat <- echoannot::NOTT2019_get_epigenomic_peaks(convert_to_granges = TRUE)
    # nott_gr <- GenomicRanges::split(x = nott_dat,
    #                                 f = list(nott_dat$Cell_type))
    # query_granges2 <- regioneR::filterChromosomes(regioneR::getGenome(genome = "hg19"),
    #                                               keep.chr = c("chr6","chr7","chr10"))
    # query_granges2 <- echodata::dt_to_granges(query_granges2, style = "NCBI")
    # enrich2 <- test_enrichment(grlist1 = nott_gr["astrocytes"],
    #                            grlist2 = nott_gr["microglia"],
    #                            genome = query_granges2,
    #                            ntimes = 100)
    # testthat::expect_true(all(enrich$gr1==ids1))
    # testthat::expect_true(all(enrich$gr2==ids2))
    # testthat::expect_equal(nrow(enrich), length(ids1)*length(ids2))
})
