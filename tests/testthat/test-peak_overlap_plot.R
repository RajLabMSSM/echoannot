test_that("peak_overlap_plot works", {
    gg_epi <- echoannot::peak_overlap_plot(
        merged_DT = echodata::get_Nalls2019_merged(),
        include.NOTT_2019_enhancers_promoters = TRUE,
        include.NOTT_2019_PLACseq = TRUE,
        #### Omit many annot to save time ####
        include.NOTT_2019_peaks = TRUE,
        include.CORCES_2020_scATACpeaks = TRUE,
        include.CORCES_2020_Cicero_coaccess = TRUE,
        include.CORCES_2020_bulkATACpeaks = TRUE,
        include.CORCES_2020_HiChIP_FitHiChIP_coaccess = TRUE,
        include.CORCES_2020_gene_annotations = TRUE
    )
    testthat::expect_length(gg_epi, 2)
    testthat::expect_true(methods::is(gg_epi$plot, "gg"))
    testthat::expect_true(methods::is(gg_epi$data, "data.frame"))
    testthat::expect_gte(nrow(gg_epi$data), 100)
})
