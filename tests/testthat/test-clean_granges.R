test_that("clean_granges removes reserved column names", {
    gr <- GenomicRanges::GRanges(
        seqnames = c("chr1", "chr1"),
        ranges = IRanges::IRanges(start = c(100, 200), end = c(150, 250))
    )
    GenomicRanges::mcols(gr)$element <- c("foo", "bar")
    GenomicRanges::mcols(gr)$start <- 1:2
    GenomicRanges::mcols(gr)$real_data <- c("a", "b")
    GenomicRanges::mcols(gr)$keep_also <- c("x", "y")

    result <- echoannot:::clean_granges(gr)
    mcol_names <- colnames(GenomicRanges::mcols(result))
    ## Reserved columns should be removed
    expect_false("element" %in% mcol_names)
    expect_false("start" %in% mcol_names)
    ## Non-reserved columns should remain
    expect_true("real_data" %in% mcol_names)
    expect_true("keep_also" %in% mcol_names)
})

test_that("clean_granges preserves non-reserved columns", {
    gr <- GenomicRanges::GRanges(
        seqnames = "chr1",
        ranges = IRanges::IRanges(start = 100, end = 200)
    )
    GenomicRanges::mcols(gr)$SNP <- "rs123"
    GenomicRanges::mcols(gr)$Cell_type <- "neurons"

    result <- echoannot:::clean_granges(gr)
    mcol_names <- colnames(GenomicRanges::mcols(result))
    expect_true("SNP" %in% mcol_names)
    expect_true("Cell_type" %in% mcol_names)
})

test_that("clean_granges returns GRanges", {
    gr <- GenomicRanges::GRanges(
        seqnames = "chr1",
        ranges = IRanges::IRanges(start = 100, end = 200)
    )
    GenomicRanges::mcols(gr)$SNP <- "rs123"
    GenomicRanges::mcols(gr)$Cell_type <- "neurons"

    result <- echoannot:::clean_granges(gr)
    expect_true(methods::is(result, "GRanges"))
    expect_equal(length(result), 1)
})
