test_that("convert_plots works", {
  
    #### Create example plot_list ####
    gg <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, cyl)) + 
        ggplot2::geom_point()
    plot_list <- lapply(seq_len(3), function(x) gg)

    ##### As ggplot #####
    return_out1 <- echoannot::convert_plots(plot_list = plot_list,
                                            return_as = "ggplot")
    ##### As ggbio #####
    return_out2 <- echoannot::convert_plots(plot_list = plot_list,
                                            return_as = "ggbio")
    ##### As patchwork #####
    return_out3 <- echoannot::convert_plots(plot_list = plot_list,
                                            return_as = "patchwork", 
                                            x_limits = c(15, 30))
    ##### As Tracks #####
    return_out4 <- echoannot::convert_plots(plot_list = plot_list,
                                            return_as = "Tracks")
    ##### From Tracks #####
    return_out5 <- echoannot::convert_plots(plot_list = return_out4,
                                            return_as = "ggplot", 
                                            x_limits = c(15, 30))
    
    testthat::expect_true(methods::is(return_out1,"list"))
    testthat::expect_true(methods::is(return_out1[[1]],"ggplot"))
    testthat::expect_true(methods::is(return_out2,"list"))
    testthat::expect_true(methods::is(return_out2[[1]],"GGbio"))
    testthat::expect_true(methods::is(return_out3,"patchwork")) 
    testthat::expect_true(methods::is(return_out4,"Tracks"))
    testthat::expect_true(methods::is(return_out5,"list"))
    testthat::expect_true(!methods::is(return_out5,"Tracks"))
    testthat::expect_true(methods::is(return_out5[[1]],"ggplot"))
})
