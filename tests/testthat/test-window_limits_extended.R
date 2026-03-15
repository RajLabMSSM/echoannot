test_that("get_window_limits with zoom='1x' returns full range", {
    dat <- echodata::BST1
    result <- echoannot::get_window_limits(
        dat = dat,
        zoom = "1x",
        genomic_units = "POS",
        verbose = FALSE
    )
    expect_length(result, 2)
    expect_equal(result[1], min(dat$POS, na.rm = TRUE))
    expect_equal(result[2], max(dat$POS, na.rm = TRUE))
})

test_that("get_window_limits with zoom='all' returns full range", {
    dat <- echodata::BST1
    result <- echoannot::get_window_limits(
        dat = dat,
        zoom = "all",
        genomic_units = "POS",
        verbose = FALSE
    )
    expect_length(result, 2)
    expect_equal(result[1], min(dat$POS, na.rm = TRUE))
    expect_equal(result[2], max(dat$POS, na.rm = TRUE))
})

test_that("get_window_limits with zoom='2x' returns narrower range", {
    dat <- echodata::BST1
    result_1x <- echoannot::get_window_limits(
        dat = dat,
        zoom = "1x",
        genomic_units = "POS",
        verbose = FALSE
    )
    result_2x <- echoannot::get_window_limits(
        dat = dat,
        zoom = "2x",
        genomic_units = "POS",
        verbose = FALSE
    )
    ## 2x zoom should produce a narrower window
    range_1x <- result_1x[2] - result_1x[1]
    range_2x <- result_2x[2] - result_2x[1]
    expect_true(range_2x < range_1x)
})

test_that("get_window_limits with Mb units converts correctly", {
    dat <- echodata::BST1
    result_pos <- echoannot::get_window_limits(
        dat = dat,
        zoom = "1x",
        genomic_units = "POS",
        verbose = FALSE
    )
    result_mb <- echoannot::get_window_limits(
        dat = dat,
        zoom = "1x",
        genomic_units = "Mb",
        verbose = FALSE
    )
    expect_equal(result_mb[1], result_pos[1] / 1000000)
    expect_equal(result_mb[2], result_pos[2] / 1000000)
})

test_that("get_window_limits with basepair zoom", {
    dat <- echodata::BST1
    result <- echoannot::get_window_limits(
        dat = dat,
        zoom = 50000,
        genomic_units = "POS",
        verbose = FALSE
    )
    expect_length(result, 2)
    window_size <- result[2] - result[1]
    expect_equal(window_size, 50000L)
})

test_that("get_window_limits NULL zoom defaults to 1x", {
    dat <- echodata::BST1
    result_null <- echoannot::get_window_limits(
        dat = dat,
        zoom = NULL,
        genomic_units = "POS",
        verbose = FALSE
    )
    result_1x <- echoannot::get_window_limits(
        dat = dat,
        zoom = "1x",
        genomic_units = "POS",
        verbose = FALSE
    )
    expect_equal(result_null, result_1x)
})

test_that("get_window_limits with multiple zooms returns list", {
    dat <- echodata::BST1
    result <- echoannot::get_window_limits(
        dat = dat,
        zoom = c("1x", "2x", "all"),
        genomic_units = "POS",
        verbose = FALSE
    )
    expect_true(is.list(result))
    expect_length(result, 3)
    expect_true(all(c("1x", "2x", "all") %in% names(result)))
})
