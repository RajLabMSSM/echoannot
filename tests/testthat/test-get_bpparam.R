test_that("get_bpparam returns BPPARAM object", {
    skip_if_not_installed("BiocParallel")
    result <- echoannot:::get_bpparam(workers = 1)
    expect_true(methods::is(result, "BiocParallelParam"))
})

test_that("get_bpparam with register_now=FALSE returns without registering", {
    skip_if_not_installed("BiocParallel")
    result <- echoannot:::get_bpparam(workers = 1, register_now = FALSE)
    expect_true(methods::is(result, "BiocParallelParam"))
})

test_that("get_bpparam with register_now=TRUE returns object", {
    skip_if_not_installed("BiocParallel")
    result <- echoannot:::get_bpparam(workers = 1, register_now = TRUE)
    expect_true(methods::is(result, "BiocParallelParam"))
})

test_that("get_bpparam selects appropriate backend for OS", {
    skip_if_not_installed("BiocParallel")
    result <- echoannot:::get_bpparam(workers = 2)
    if (.Platform$OS.type == "windows") {
        expect_true(methods::is(result, "SerialParam"))
    } else {
        expect_true(methods::is(result, "SnowParam"))
    }
})
