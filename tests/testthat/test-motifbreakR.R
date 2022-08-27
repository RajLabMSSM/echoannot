test_that("MOTIFBREAKR works", {
  
    library(BSgenome) ## <-- IMPORTANT!
    set.seed(2022)
    #### Example fine-mapping results ####
    merged_DT <- echodata::get_Nalls2019_merged()
    #### Run motif analyses + calculate p-values ####
    mb_res <- MOTIFBREAKR(rsid_list = c("rs11175620"),
                          # limit the number of datasets tested
                          # for demonstration purposes only
                          pwmList_max = 5, 
                          calculate_pvals = TRUE)
    testthat::expect_true(methods::is(mb_res,"GRanges"))
    testthat::expect_length(mb_res,1)
    testthat::expect_true("alleleDiff" %in% names(GenomicRanges::mcols(mb_res)))
    testthat::expect_true(
        echoannot:::MOTIFBREAKR_has_pvals(mb_res)
    )
      
    #### Filter and merge results #### 
    mb_res_filt <- MOTIFBREAKR_filter(mb_res = mb_res_p,
                                      merged_DT = merged_DT, 
                                      top_geneSymbol_hits = 1)
    testthat::expect_true(methods::is(mb_res_filt,"data.table"))
    testthat::expect_true(
        echoannot:::MOTIFBREAKR_has_pvals(mb_res = mb_res_p)
    )
    
    
    #### Plot ####  
    #### Select genome build #### 
    library(BSgenome.Hsapiens.UCSC.hg19) ## IMPORTANT! 
    #### With p-values ####
    plot_paths <- MOTIFBREAKR_plot(mb_res = mb_res_p)
    testthat::expect_true(file.exists(plot_paths$rs11175620))
    #### Without p-values ####
    plot_paths <- MOTIFBREAKR_plot(mb_res = mb_res)
    testthat::expect_true(file.exists(plot_paths$rs11175620)) 
})
