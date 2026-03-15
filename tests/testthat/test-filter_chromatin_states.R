test_that("filter_chromatin_states annotates chromatin states", {
    ## Create a GRanges with name column formatted like ROADMAP data
    gr <- GenomicRanges::GRanges(
        seqnames = c("chr1", "chr1", "chr1"),
        ranges = IRanges::IRanges(
            start = c(100, 200, 300),
            end = c(150, 250, 350)
        )
    )
    GenomicRanges::mcols(gr)$name <- c("1_TssA", "2_Enh", "3_Quies")

    result <- echoannot:::filter_chromatin_states(
        grl = list(test = gr),
        chrom_states = NULL,
        verbose = FALSE
    )
    expect_true(methods::is(result, "GRangesList"))
    gr_out <- result[[1]]
    expect_true("chrom_type" %in% colnames(GenomicRanges::mcols(gr_out)))
    expect_true("chrom_state" %in% colnames(GenomicRanges::mcols(gr_out)))
    expect_equal(gr_out$chrom_type, c("TssA", "Enh", "Quies"))
})

test_that("filter_chromatin_states filters to specific states", {
    gr <- GenomicRanges::GRanges(
        seqnames = c("chr1", "chr1", "chr1"),
        ranges = IRanges::IRanges(
            start = c(100, 200, 300),
            end = c(150, 250, 350)
        )
    )
    GenomicRanges::mcols(gr)$name <- c("1_TssA", "2_Enh", "3_Quies")

    result <- echoannot:::filter_chromatin_states(
        grl = list(test = gr),
        chrom_states = c("TssA"),
        verbose = FALSE
    )
    gr_out <- result[[1]]
    expect_equal(length(gr_out), 1)
    expect_equal(gr_out$chrom_type, "TssA")
})

test_that("filter_chromatin_states handles single GRanges input", {
    gr <- GenomicRanges::GRanges(
        seqnames = "chr1",
        ranges = IRanges::IRanges(start = 100, end = 200)
    )
    GenomicRanges::mcols(gr)$name <- "1_TssA"

    result <- echoannot:::filter_chromatin_states(
        grl = gr,
        chrom_states = NULL,
        verbose = FALSE
    )
    expect_true(methods::is(result, "GRangesList"))
})
