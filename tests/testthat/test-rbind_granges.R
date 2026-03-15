test_that("rbind_granges merges GRanges with shared columns", {
    gr1 <- GenomicRanges::GRanges(
        seqnames = "chr1",
        ranges = IRanges::IRanges(start = 100, end = 200)
    )
    GenomicRanges::mcols(gr1)$SNP <- "rs1"
    GenomicRanges::mcols(gr1)$Assay <- "ATAC"

    gr2 <- GenomicRanges::GRanges(
        seqnames = "chr1",
        ranges = IRanges::IRanges(start = 300, end = 400)
    )
    GenomicRanges::mcols(gr2)$SNP <- "rs2"
    GenomicRanges::mcols(gr2)$Assay <- "H3K27ac"

    result <- echoannot:::rbind_granges(gr1, gr2)
    expect_true(methods::is(result, "GRanges"))
    expect_equal(length(result), 2)
    expect_equal(result$SNP, c("rs1", "rs2"))
})

test_that("rbind_granges drops non-shared columns", {
    gr1 <- GenomicRanges::GRanges(
        seqnames = "chr1",
        ranges = IRanges::IRanges(start = 100, end = 200)
    )
    GenomicRanges::mcols(gr1)$SNP <- "rs1"
    GenomicRanges::mcols(gr1)$extra1 <- "only_in_gr1"

    gr2 <- GenomicRanges::GRanges(
        seqnames = "chr1",
        ranges = IRanges::IRanges(start = 300, end = 400)
    )
    GenomicRanges::mcols(gr2)$SNP <- "rs2"
    GenomicRanges::mcols(gr2)$extra2 <- "only_in_gr2"

    result <- echoannot:::rbind_granges(gr1, gr2)
    mcol_names <- colnames(GenomicRanges::mcols(result))
    expect_true("SNP" %in% mcol_names)
    expect_false("extra1" %in% mcol_names)
    expect_false("extra2" %in% mcol_names)
})

test_that("rbind_granges with identical mcols", {
    gr1 <- GenomicRanges::GRanges(
        seqnames = "chr1",
        ranges = IRanges::IRanges(start = 100, end = 200)
    )
    GenomicRanges::mcols(gr1)$SNP <- "rs1"

    gr2 <- GenomicRanges::GRanges(
        seqnames = "chr2",
        ranges = IRanges::IRanges(start = 100, end = 200)
    )
    GenomicRanges::mcols(gr2)$SNP <- "rs2"

    result <- echoannot:::rbind_granges(gr1, gr2)
    expect_equal(length(result), 2)
    expect_true("SNP" %in% colnames(GenomicRanges::mcols(result)))
})
