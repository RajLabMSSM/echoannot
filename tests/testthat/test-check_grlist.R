test_that("check_grlist converts GRanges to list", {
    gr <- GenomicRanges::GRanges(
        seqnames = "chr1",
        ranges = IRanges::IRanges(start = 100, end = 200)
    )
    result <- echoannot:::check_grlist(grlist = gr, verbose = FALSE)
    expect_true(is.list(result))
})

test_that("check_grlist converts data.frame to list of GRanges", {
    dat <- data.frame(
        CHR = 1,
        POS = 1000,
        SNP = "rs1"
    )
    result <- echoannot:::check_grlist(
        grlist = dat,
        style = "UCSC",
        verbose = FALSE
    )
    expect_true(is.list(result))
})

test_that("check_grlist converts GRangesList to list", {
    gr1 <- GenomicRanges::GRanges(
        seqnames = "chr1",
        ranges = IRanges::IRanges(start = 100, end = 200)
    )
    gr2 <- GenomicRanges::GRanges(
        seqnames = "chr2",
        ranges = IRanges::IRanges(start = 300, end = 400)
    )
    grl <- GenomicRanges::GRangesList(a = gr1, b = gr2)
    result <- echoannot:::check_grlist(grlist = grl, verbose = FALSE)
    expect_true(is.list(result))
})

test_that("check_grlist adds names when list has none", {
    gr1 <- GenomicRanges::GRanges(
        seqnames = "chr1",
        ranges = IRanges::IRanges(start = 100, end = 200)
    )
    GenomicRanges::mcols(gr1)$CHR <- 1
    GenomicRanges::mcols(gr1)$POS <- 100
    result <- echoannot:::check_grlist(
        grlist = list(gr1),
        prefix = "test",
        verbose = FALSE
    )
    expect_true(!is.null(names(result)))
})

test_that("check_grlist errors on invalid input type", {
    expect_error(
        echoannot:::check_grlist(grlist = "invalid_string", verbose = FALSE),
        "grlist must be one of"
    )
})
