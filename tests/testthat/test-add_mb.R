test_that("add_mb adds Mb column to data.table", {
    dat <- echodata::BST1
    result <- echoannot::add_mb(dat = dat, pos_col = "POS")
    expect_true("Mb" %in% colnames(result))
    expect_equal(result$Mb[1], dat$POS[1] / 1e6)
})

test_that("add_mb does not overwrite existing Mb column", {
    dat <- data.table::copy(echodata::BST1)
    dat$Mb <- 999
    result <- echoannot::add_mb(dat = dat, pos_col = "POS")
    expect_true(all(result$Mb == 999))
})

test_that("add_mb works with data.frame", {
    dat <- as.data.frame(echodata::BST1[1:10, ])
    result <- echoannot::add_mb(dat = dat, pos_col = "POS")
    expect_true("Mb" %in% colnames(result))
    expect_equal(result$Mb[1], dat$POS[1] / 1e6)
})

test_that("add_mb works with GRanges", {
    dat <- echodata::BST1[1:10, ]
    gr <- echodata::dt_to_granges(
        dat = dat,
        chrom_col = "CHR",
        start_col = "POS",
        style = "NCBI"
    )
    ## POS becomes the start coordinate and is not in mcols;
    ## add_mb should fall back to start(gr).
    result <- echoannot::add_mb(dat = gr, pos_col = "POS")
    expect_true("Mb" %in% colnames(GenomicRanges::mcols(result)))
    expect_equal(
        GenomicRanges::mcols(result)[["Mb"]],
        GenomicRanges::start(gr) / 1e6
    )
})
