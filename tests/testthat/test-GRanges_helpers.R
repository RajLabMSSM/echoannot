test_that("clean_granges removes internal columns", {
    gr <- GenomicRanges::GRanges(
        seqnames = "chr1",
        ranges = IRanges::IRanges(start = c(100, 200), end = c(150, 250))
    )
    GenomicRanges::mcols(gr)$myCol <- c("A", "B")
    GenomicRanges::mcols(gr)$start <- c(100, 200)
    GenomicRanges::mcols(gr)$end <- c(150, 250)

    result <- echoannot:::clean_granges(gr)
    mc <- GenomicRanges::mcols(result)
    expect_true("myCol" %in% colnames(mc))
    expect_false("start" %in% colnames(mc))
    expect_false("end" %in% colnames(mc))
})

test_that("clean_granges preserves user columns", {
    gr <- GenomicRanges::GRanges(
        seqnames = "chr1",
        ranges = IRanges::IRanges(start = 100, end = 200)
    )
    GenomicRanges::mcols(gr)$SNP <- "rs123"
    GenomicRanges::mcols(gr)$P <- 0.05
    result <- echoannot:::clean_granges(gr)
    mc <- GenomicRanges::mcols(result)
    expect_true("SNP" %in% colnames(mc))
    expect_true("P" %in% colnames(mc))
})

test_that("rbind_granges merges GRanges with different mcols", {
    gr1 <- GenomicRanges::GRanges(
        seqnames = "chr1",
        ranges = IRanges::IRanges(start = 100, end = 200)
    )
    GenomicRanges::mcols(gr1)$shared <- "A"
    GenomicRanges::mcols(gr1)$only_gr1 <- "X"

    gr2 <- GenomicRanges::GRanges(
        seqnames = "chr1",
        ranges = IRanges::IRanges(start = 300, end = 400)
    )
    GenomicRanges::mcols(gr2)$shared <- "B"
    GenomicRanges::mcols(gr2)$only_gr2 <- "Y"

    result <- echoannot:::rbind_granges(gr1, gr2)
    expect_true(methods::is(result, "GRanges"))
    expect_equal(length(result), 2)
    expect_true("shared" %in% colnames(GenomicRanges::mcols(result)))
    # Non-shared columns should be dropped
    expect_false("only_gr1" %in% colnames(GenomicRanges::mcols(result)))
    expect_false("only_gr2" %in% colnames(GenomicRanges::mcols(result)))
})

test_that("rbind_granges with identical mcols preserves all", {
    gr1 <- GenomicRanges::GRanges(
        seqnames = "chr1",
        ranges = IRanges::IRanges(start = 100, end = 200)
    )
    GenomicRanges::mcols(gr1)$col1 <- "A"
    GenomicRanges::mcols(gr1)$col2 <- 1

    gr2 <- GenomicRanges::GRanges(
        seqnames = "chr2",
        ranges = IRanges::IRanges(start = 300, end = 400)
    )
    GenomicRanges::mcols(gr2)$col1 <- "B"
    GenomicRanges::mcols(gr2)$col2 <- 2

    result <- echoannot:::rbind_granges(gr1, gr2)
    expect_equal(length(result), 2)
    mc <- GenomicRanges::mcols(result)
    expect_true(all(c("col1", "col2") %in% colnames(mc)))
})

test_that("check_grlist handles data.frame input", {
    dat <- as.data.frame(echodata::BST1[1:5, ])
    result <- echoannot:::check_grlist(dat, style = "NCBI")
    expect_true(is.list(result))
    expect_true(methods::is(result[[1]], "GRanges"))
})

test_that("check_grlist handles GRanges input", {
    gr <- GenomicRanges::GRanges(
        seqnames = "1",
        ranges = IRanges::IRanges(start = 100, end = 200)
    )
    result <- echoannot:::check_grlist(gr, style = "NCBI")
    expect_true(is.list(result))
})

test_that("check_grlist errors on invalid input", {
    expect_error(
        echoannot:::check_grlist("invalid"),
        "grlist must be"
    )
})

test_that("name_filter_convert filters by min_hits", {
    gr1 <- GenomicRanges::GRanges(
        seqnames = c("chr1", "chr1"),
        ranges = IRanges::IRanges(start = c(100, 200), end = c(150, 250))
    )
    gr2 <- GenomicRanges::GRanges(
        seqnames = character(0),
        ranges = IRanges::IRanges()
    )
    grl <- list(has_hits = gr1, empty = gr2)
    result <- echoannot:::name_filter_convert(grl, min_hits = 1)
    expect_true(methods::is(result, "GRangesList"))
    expect_true(length(result) >= 1)
})

test_that("name_filter_convert removes NULL entries", {
    gr1 <- GenomicRanges::GRanges(
        seqnames = "chr1",
        ranges = IRanges::IRanges(start = 100, end = 200)
    )
    grl <- list(valid = gr1, null_entry = NULL)
    result <- echoannot:::name_filter_convert(grl, min_hits = 1)
    expect_true(methods::is(result, "GRangesList"))
})
