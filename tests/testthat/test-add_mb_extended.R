test_that("add_mb adds Mb column to data.frame", {
    dat <- data.frame(
        POS = c(1000000, 2000000, 3000000),
        SNP = c("rs1", "rs2", "rs3")
    )
    result <- echoannot::add_mb(dat = dat, pos_col = "POS")
    expect_true("Mb" %in% colnames(result))
    expect_equal(result$Mb, c(1, 2, 3))
})

test_that("add_mb adds Mb column to data.table", {
    dat <- data.table::data.table(
        POS = c(1500000, 2500000),
        SNP = c("rs1", "rs2")
    )
    result <- echoannot::add_mb(dat = dat, pos_col = "POS")
    expect_true("Mb" %in% colnames(result))
    expect_equal(result$Mb, c(1.5, 2.5))
})

test_that("add_mb does not overwrite existing Mb column", {
    dat <- data.frame(
        POS = c(1000000, 2000000),
        Mb = c(99, 99)
    )
    result <- echoannot::add_mb(dat = dat, pos_col = "POS")
    ## Should keep the existing Mb values unchanged
    expect_equal(result$Mb, c(99, 99))
})

test_that("add_mb with echodata::BST1", {
    dat <- echodata::BST1
    result <- echoannot::add_mb(dat = dat, pos_col = "POS")
    expect_true("Mb" %in% colnames(result))
    ## All Mb values should be POS / 1e6
    expected_mb <- dat$POS / 1000000
    expect_equal(result$Mb, expected_mb)
})
