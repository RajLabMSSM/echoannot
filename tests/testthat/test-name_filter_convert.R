test_that("name_filter_convert filters by min_hits", {
    gr1 <- GenomicRanges::GRanges(
        seqnames = c("chr1", "chr1", "chr1"),
        ranges = IRanges::IRanges(start = c(100, 200, 300), end = c(150, 250, 350))
    )
    gr2 <- GenomicRanges::GRanges(
        seqnames = "chr2",
        ranges = IRanges::IRanges(start = 100, end = 200)
    )
    grl <- list(item1 = gr1, item2 = gr2)

    result <- echoannot:::name_filter_convert(grl, min_hits = 2)
    expect_true(methods::is(result, "GRangesList"))
    ## gr1 has 3 ranges (>= 2), gr2 has 1 (< 2), so only gr1 remains
    expect_equal(length(result), 1)
})

test_that("name_filter_convert keeps all with min_hits=1", {
    gr1 <- GenomicRanges::GRanges(
        seqnames = "chr1",
        ranges = IRanges::IRanges(start = 100, end = 200)
    )
    gr2 <- GenomicRanges::GRanges(
        seqnames = "chr2",
        ranges = IRanges::IRanges(start = 100, end = 200)
    )
    grl <- list(item1 = gr1, item2 = gr2)

    result <- echoannot:::name_filter_convert(grl, min_hits = 1)
    expect_true(methods::is(result, "GRangesList"))
    expect_equal(length(result), 2)
})

test_that("name_filter_convert keeps all with min_hits=NULL", {
    gr1 <- GenomicRanges::GRanges(
        seqnames = "chr1",
        ranges = IRanges::IRanges(start = 100, end = 200)
    )
    gr2 <- GenomicRanges::GRanges(
        seqnames = "chr2",
        ranges = IRanges::IRanges(start = 100, end = 200)
    )
    grl <- list(item1 = gr1, item2 = gr2)

    result <- echoannot:::name_filter_convert(grl, min_hits = NULL)
    expect_true(methods::is(result, "GRangesList"))
    expect_equal(length(result), 2)
})

test_that("name_filter_convert removes NULL entries", {
    gr1 <- GenomicRanges::GRanges(
        seqnames = "chr1",
        ranges = IRanges::IRanges(start = 100, end = 200)
    )
    grl <- list(item1 = gr1, item2 = NULL)

    result <- echoannot:::name_filter_convert(grl, min_hits = 1)
    expect_true(methods::is(result, "GRangesList"))
    expect_equal(length(result), 1)
})

test_that("name_filter_convert returns GRangesList type", {
    gr1 <- GenomicRanges::GRanges(
        seqnames = "chr1",
        ranges = IRanges::IRanges(start = 100, end = 200)
    )
    grl <- list(item1 = gr1)
    result <- echoannot:::name_filter_convert(grl, min_hits = 1)
    expect_true(methods::is(result, "GRangesList"))
})
