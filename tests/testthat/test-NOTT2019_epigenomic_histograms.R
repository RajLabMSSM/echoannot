test_that("NOTT2019_epigenomic_histograms works", {
  
    dat <- echodata::BST1
    items_lv1 <- c("data","plot")
    items_lv2 <- c("raw","peaks")
    #### Full pipelne ####
    nott2019_track <- echoannot:: NOTT2019_epigenomic_histograms(
        dat = dat)
    testthat::expect_true(methods::is(nott2019_track$plot,"AssayData"))
    testthat::expect_true(methods::is(nott2019_track$data$raw,"GRanges"))
    testthat::expect_gte(length(nott2019_track$data$raw),32000)
    testthat::expect_true(methods::is(nott2019_track$data$peaks,"GRanges"))
    testthat::expect_gte(length(nott2019_track$data$peaks),500)
    
    #### Early exit  ####
    nott2019_assay_track <- echoannot::NOTT2019_epigenomic_histograms(
        dat = dat, 
        return_assay_track = TRUE,
        ## Supply subset of metadata 
        bigwig_metadata = echoannot::NOTT2019_bigwig_metadata[1:2,])
    testthat::expect_true(methods::is(nott2019_assay_track$plot,"ggplot"))
    testthat::expect_true(methods::is(nott2019_assay_track$data$raw,"GRanges"))
    testthat::expect_gte(length(nott2019_assay_track$data$raw),30000)
    testthat::expect_true(methods::is(nott2019_assay_track$data$peaks,"GRanges"))
    testthat::expect_gte(length(nott2019_assay_track$data$peaks),500)
})
