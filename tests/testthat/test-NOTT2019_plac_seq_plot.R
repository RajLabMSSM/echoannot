test_that("NOTT2019_plac_seq_plot works", {
    
    dat <- echodata::BST1
    locus_dir <- file.path(tempdir(), echodata::locus_dir)
    
    #### Full window ####
    trks_plus_lines <- echoannot::NOTT2019_plac_seq_plot(
        dat = dat
    )
    testthat::expect_true(methods::is(trks_plus_lines,"Tracks"))
    gg_trk <- echoannot:::tracks_to_ggplot_list(trks_plus_lines)
    for(x in names(trks_plus_lines)){
        testthat::expect_true(methods::is(trks_plus_lines[[x]],"gg"))
    }  
    
    #### Zoom in ####
    ## numeric zoom_window
    trks_plus_lines2 <- echoannot::NOTT2019_plac_seq_plot(
        dat = dat,
        # locus_dir = tempdir(), ### Not working atm
        zoom_window = 250000,
        highlight_plac = FALSE # test uniform opacity as well
    )
    ## character zoom_window
    testthat::expect_error(
        trks_plus_lines2 <- echoannot::NOTT2019_plac_seq_plot(
            dat = dat,
            zoom_window = "5x"
        )
    )
    
    #### Interaction plot only ####
    trks_interaction <- echoannot::NOTT2019_plac_seq_plot(
        dat = dat, 
        return_interaction_track = TRUE
    )
    testthat::expect_true(methods::is(trks_interaction,"Tracks"))
})
