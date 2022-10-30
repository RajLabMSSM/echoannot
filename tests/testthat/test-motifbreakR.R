test_that("MOTIFBREAKR works", {
  
    library(BSgenome) ## <-- IMPORTANT! 
    set.seed(2022)
    #### Example fine-mapping results ####
    merged_DT <- echodata::get_Nalls2019_merged()
    #### Run motif analyses + calculate p-values ####
    mb_res <- MOTIFBREAKR(rsid_list = c("rs11175620"),
                          # limit the number of datasets tested
                          # for demonstration purposes only
                          pwmList_max = 4, 
                          calculate_pvals = TRUE,
                          force_new = TRUE)
    testthat::expect_true(methods::is(mb_res,"GRanges"))
    testthat::expect_length(mb_res,1)
    testthat::expect_true("alleleDiff" %in% names(GenomicRanges::mcols(mb_res)))
    testthat::expect_true(
        echoannot:::MOTIFBREAKR_has_pvals(mb_res)
    )
      
    #### Filter and merge results #### 
    mb_merge <- MOTIFBREAKR_filter(mb_res = mb_res,
                                   merged_DT = merged_DT, 
                                   pvalue_threshold = NULL,
                                   qvalue_threshold = NULL,
                                   top_geneSymbol_hits = 1)
    testthat::expect_true(methods::is(mb_merge,"data.table"))
    testthat::expect_true(
        echoannot:::MOTIFBREAKR_has_pvals(mb_res = mb_merge)
    )
    
    #### Plot ####  
    library(BSgenome.Hsapiens.UCSC.hg19) ## IMPORTANT!  
    ## Sometimes causes errors during tests/examples 
    ## (but not in R console, and not on some GHA platforms) 
    plot_paths <- MOTIFBREAKR_plot(mb_res = mb_res) 
    # testthat::expect_true(file.exists(plot_paths$rs11175620)) 
    
    #### Summarise ####
    summary_ls <- MOTIFBREAKR_summarize(mb_merge = mb_merge)
    testthat::expect_equal(names(summary_ls), c("db_tally","locus_tally"))
    testthat::expect_true(methods::is(summary_ls$locus_tally,"data.table"))
    testthat::expect_equal(nrow(summary_ls$locus_tally), nrow(mb_merge))
})
