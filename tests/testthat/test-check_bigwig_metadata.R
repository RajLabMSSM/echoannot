test_that("check_bigwig_metadata passes with valid data", {
    bwm <- data.frame(
        name = "test_bigwig",
        data_link = "http://example.com/test.bw"
    )
    expect_silent(echoannot:::check_bigwig_metadata(bigwig_metadata = bwm))
})

test_that("check_bigwig_metadata errors on missing name column", {
    bwm <- data.frame(
        data_link = "http://example.com/test.bw"
    )
    expect_error(
        echoannot:::check_bigwig_metadata(bigwig_metadata = bwm),
        "Missing required columns"
    )
})

test_that("check_bigwig_metadata errors on missing data_link column", {
    bwm <- data.frame(
        name = "test_bigwig"
    )
    expect_error(
        echoannot:::check_bigwig_metadata(bigwig_metadata = bwm),
        "Missing required columns"
    )
})

test_that("check_bigwig_metadata errors on completely wrong columns", {
    bwm <- data.frame(
        foo = "bar",
        baz = "quux"
    )
    expect_error(
        echoannot:::check_bigwig_metadata(bigwig_metadata = bwm),
        "Missing required columns"
    )
})

test_that("check_bigwig_metadata with custom required_cols", {
    bwm <- data.frame(name = "test", extra_col = "foo")
    expect_silent(
        echoannot:::check_bigwig_metadata(
            bigwig_metadata = bwm,
            required_cols = c("name", "extra_col")
        )
    )
    expect_error(
        echoannot:::check_bigwig_metadata(
            bigwig_metadata = bwm,
            required_cols = c("name", "missing_col")
        ),
        "Missing required columns"
    )
})
