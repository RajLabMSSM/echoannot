test_that("granges_overlap finds overlapping positions with BST1", {
    dat1 <- echodata::BST1
    ## Create a range that overlaps with some BST1 SNPs
    dat2 <- data.frame(
        chrom = unique(dat1$CHR)[1],
        start = min(dat1$POS),
        end = min(dat1$POS) + 1000
    )
    result <- echoannot::granges_overlap(
        dat1 = dat1,
        dat2 = dat2,
        chrom_col.1 = "CHR",
        start_col.1 = "POS",
        end_col.1 = "POS",
        chrom_col.2 = "chrom",
        start_col.2 = "start",
        end_col.2 = "end",
        style = "NCBI",
        verbose = FALSE
    )
    expect_true(methods::is(result, "GRanges"))
    expect_true(length(result) >= 1)
    ## Should have merged columns from both datasets
    expect_true("SNP" %in% colnames(GenomicRanges::mcols(result)))
})

test_that("granges_overlap with xgr_example data", {
    dat1 <- echodata::BST1
    data("xgr_example", package = "echoannot")
    ## Strip mcols so it doesn't conflict
    dat2 <- xgr_example
    GenomicRanges::mcols(dat2) <- NULL

    result <- echoannot::granges_overlap(
        dat1 = dat1,
        dat2 = dat2,
        chrom_col.1 = "CHR",
        start_col.1 = "POS",
        style = "NCBI",
        verbose = FALSE
    )
    expect_true(methods::is(result, "GRanges"))
})

test_that("granges_overlap return_merged FALSE", {
    dat1 <- data.frame(
        CHR = c(1, 1),
        POS = c(100, 200)
    )
    dat2 <- data.frame(
        chrom = 1,
        start = 50,
        end = 250
    )
    result <- echoannot::granges_overlap(
        dat1 = dat1,
        dat2 = dat2,
        chrom_col.1 = "CHR",
        start_col.1 = "POS",
        end_col.1 = "POS",
        chrom_col.2 = "chrom",
        start_col.2 = "start",
        end_col.2 = "end",
        return_merged = FALSE,
        style = "NCBI",
        verbose = FALSE
    )
    expect_true(methods::is(result, "GRanges"))
    ## Without merging, should not have columns from dat1
    expect_false("POS" %in% colnames(GenomicRanges::mcols(result)))
})

test_that("granges_overlap with no overlap returns empty GRanges", {
    dat1 <- data.frame(CHR = 1, POS = 100)
    dat2 <- data.frame(chrom = 2, start = 50000, end = 60000)
    result <- echoannot::granges_overlap(
        dat1 = dat1,
        dat2 = dat2,
        chrom_col.1 = "CHR",
        start_col.1 = "POS",
        end_col.1 = "POS",
        chrom_col.2 = "chrom",
        start_col.2 = "start",
        end_col.2 = "end",
        style = "NCBI",
        verbose = FALSE
    )
    expect_true(methods::is(result, "GRanges"))
    expect_equal(length(result), 0)
})
