test_that("CS_bin_plot creates plot and data", {
    dat <- echodata::BST1
    dat$Locus <- "BST1"
    result <- suppressWarnings(
        echoannot::CS_bin_plot(merged_DT = dat, show_plot = FALSE)
    )
    expect_true(is.list(result))
    expect_true("plot" %in% names(result))
    expect_true("data" %in% names(result))
    expect_true(methods::is(result$plot, "ggplot"))
    expect_true(nrow(result$data) > 0)
})

test_that("CS_counts_plot creates plot and data", {
    dat <- echodata::BST1
    dat$Locus <- "BST1"
    result <- suppressWarnings(
        echoannot::CS_counts_plot(merged_DT = dat, show_plot = FALSE)
    )
    expect_true(is.list(result))
    expect_true("plot" %in% names(result))
    expect_true("data" %in% names(result))
    expect_true(methods::is(result$plot, "ggplot"))
})

test_that("CS_counts_plot with show_numbers=FALSE", {
    dat <- echodata::BST1
    dat$Locus <- "BST1"
    result <- suppressWarnings(
        echoannot::CS_counts_plot(
            merged_DT = dat,
            show_numbers = FALSE,
            show_plot = FALSE
        )
    )
    expect_true(methods::is(result$plot, "ggplot"))
})

test_that("CS_counts_plot label_yaxis=FALSE works", {
    dat <- echodata::BST1
    dat$Locus <- "BST1"
    result <- suppressWarnings(
        echoannot::CS_counts_plot(
            merged_DT = dat,
            label_yaxis = FALSE,
            show_plot = FALSE
        )
    )
    expect_true(methods::is(result$plot, "ggplot"))
})
