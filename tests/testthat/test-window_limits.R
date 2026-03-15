test_that("get_window_limits returns Mb by default", {
    dat <- echodata::BST1
    result <- echoannot::get_window_limits(
        dat = dat,
        zoom = "1x",
        verbose = FALSE
    )
    expect_length(result, 2)
    expect_true(result[1] < result[2])
    # In Mb, values should be small numbers
    expect_true(result[1] < 1000)
    expect_true(result[2] < 1000)
})

test_that("get_window_limits zoom='all' gives full range", {
    dat <- echodata::BST1
    result_all <- echoannot::get_window_limits(
        dat = dat,
        zoom = "all",
        verbose = FALSE
    )
    result_1x <- echoannot::get_window_limits(
        dat = dat,
        zoom = "1x",
        verbose = FALSE
    )
    expect_equal(result_all[1], result_1x[1])
    expect_equal(result_all[2], result_1x[2])
})

test_that("get_window_limits zoom numeric restricts window", {
    dat <- echodata::BST1
    result_full <- echoannot::get_window_limits(
        dat = dat,
        zoom = "1x",
        verbose = FALSE
    )
    result_narrow <- echoannot::get_window_limits(
        dat = dat,
        zoom = "5000",
        verbose = FALSE
    )
    full_span <- result_full[2] - result_full[1]
    narrow_span <- result_narrow[2] - result_narrow[1]
    expect_true(narrow_span < full_span)
})

test_that("get_window_limits zoom Nx restricts proportionally", {
    dat <- echodata::BST1
    result_1x <- echoannot::get_window_limits(
        dat = dat,
        zoom = "1x",
        verbose = FALSE
    )
    result_2x <- echoannot::get_window_limits(
        dat = dat,
        zoom = "2x",
        verbose = FALSE
    )
    span_1x <- result_1x[2] - result_1x[1]
    span_2x <- result_2x[2] - result_2x[1]
    # 2x zoom means half the span
    expect_true(abs(span_2x - span_1x / 2) < 0.001)
})

test_that("get_window_limits multiple zooms returns list", {
    dat <- echodata::BST1
    result <- echoannot::get_window_limits(
        dat = dat,
        zoom = c("1x", "2x", "50000"),
        verbose = FALSE
    )
    expect_true(is.list(result))
    expect_length(result, 3)
    expect_equal(names(result), c("1x", "2x", "50000"))
})

test_that("get_window_limits NULL zoom defaults to 1x", {
    dat <- echodata::BST1
    result_null <- echoannot::get_window_limits(
        dat = dat,
        zoom = NULL,
        verbose = FALSE
    )
    result_1x <- echoannot::get_window_limits(
        dat = dat,
        zoom = "1x",
        verbose = FALSE
    )
    expect_equal(result_null, result_1x)
})
