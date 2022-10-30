test_that("super_summary_plot works", {
    
    #### Import data ####
    merged_DT <- echodata::get_Nalls2019_merged()
    
    #### Test: CS_bin_plot ####
    gg_cs_bin <- echoannot::CS_bin_plot(merged_DT = merged_DT,
                                        show_plot = FALSE)
    testthat::expect_length(gg_cs_bin, 2)
    testthat::expect_true(methods::is(gg_cs_bin$plot, "gg"))
    testthat::expect_true(methods::is(gg_cs_bin$data, "data.frame"))
    testthat::expect_gte(nrow(gg_cs_bin$data), 400)
    
    #### Test: CS_counts_plot ####
    gg_cs_counts <- echoannot::CS_counts_plot(merged_DT = merged_DT, 
                                              show_plot = FALSE)
    testthat::expect_length(gg_cs_counts, 2)
    testthat::expect_true(methods::is(gg_cs_counts$plot, "gg"))
    testthat::expect_true(methods::is(gg_cs_counts$data, "data.frame"))
    testthat::expect_gte(nrow(gg_cs_counts$data), 300)
    
    #### Test: peak_overlap_plot ####
    gg_epi <- echoannot::peak_overlap_plot(
        merged_DT = merged_DT,
        include.NOTT2019_enhancers_promoters = TRUE,
        include.NOTT2019_PLACseq = TRUE, 
        include.NOTT2019_peaks = TRUE,
        include.CORCES2020_scATACpeaks = TRUE,
        include.CORCES2020_Cicero_coaccess = TRUE,
        include.CORCES2020_bulkATACpeaks = TRUE,
        include.CORCES2020_HiChIP_FitHiChIP_coaccess = TRUE,
        include.CORCES2020_gene_annotations = TRUE,
        show_plot = FALSE
    )
    testthat::expect_length(gg_epi, 3)
    testthat::expect_true(methods::is(gg_epi$plot, "gg"))
    testthat::expect_true(methods::is(gg_epi$data, "data.frame"))
    testthat::expect_gte(nrow(gg_epi$data), 100)
    
    
    #### Test: peak_overlap_plot ####
    super_plot <- echoannot::super_summary_plot(merged_DT = merged_DT, 
                                                plot_missense = FALSE, 
                                                show_plot = FALSE)
    testthat::expect_length(super_plot, 2)
    testthat::expect_true(methods::is(super_plot$plot, "gg"))
    testthat::expect_true(methods::is(super_plot$data, "data.frame"))
    testthat::expect_gte(nrow(super_plot$data), 100)
})
