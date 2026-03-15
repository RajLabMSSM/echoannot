test_that("XGR_merge_and_process processes GRangesList", {
    ## Build a minimal GRangesList with names using "." separator
    ## (matching ENCODE_TFBS_ClusteredV3_CellTypes pattern)
    gr1 <- GenomicRanges::GRanges(
        seqnames = rep("chr4", 3),
        ranges = IRanges::IRanges(start = c(100, 200, 300),
                                   end = c(150, 250, 350))
    )
    names(gr1) <- rep("CellA.H3K27ac", 3)
    gr2 <- GenomicRanges::GRanges(
        seqnames = rep("chr4", 3),
        ranges = IRanges::IRanges(start = c(400, 500, 600),
                                   end = c(450, 550, 650))
    )
    names(gr2) <- rep("CellB.ATAC", 3)
    grl_xgr <- GenomicRanges::GRangesList(test1 = gr1, test2 = gr2)

    result <- echoannot:::XGR_merge_and_process(
        grl.xgr = grl_xgr,
        lib = "ENCODE_TFBS_ClusteredV3_CellTypes",
        n_top_sources = 10
    )
    expect_true(methods::is(result, "GRanges"))
    expect_true("Source" %in% colnames(GenomicRanges::mcols(result)))
    expect_true("Assay" %in% colnames(GenomicRanges::mcols(result)))
    expect_true("Source_Assay" %in% colnames(GenomicRanges::mcols(result)))
    expect_true("Start" %in% colnames(GenomicRanges::mcols(result)))
    expect_true("End" %in% colnames(GenomicRanges::mcols(result)))
})

test_that("XGR_merge_and_process respects n_top_sources", {
    ## Create GRangesList with multiple distinct sources
    gr_a <- GenomicRanges::GRanges(
        seqnames = rep("chr1", 5),
        ranges = IRanges::IRanges(start = seq(100, 500, 100),
                                   end = seq(150, 550, 100))
    )
    names(gr_a) <- rep("SourceA.Assay1", 5)

    gr_b <- GenomicRanges::GRanges(
        seqnames = rep("chr1", 3),
        ranges = IRanges::IRanges(start = seq(100, 300, 100),
                                   end = seq(150, 350, 100))
    )
    names(gr_b) <- rep("SourceB.Assay2", 3)

    gr_c <- GenomicRanges::GRanges(
        seqnames = "chr1",
        ranges = IRanges::IRanges(start = 100, end = 150)
    )
    names(gr_c) <- "SourceC.Assay3"

    grl <- GenomicRanges::GRangesList(a = gr_a, b = gr_b, c = gr_c)

    result <- echoannot:::XGR_merge_and_process(
        grl.xgr = grl,
        lib = "ENCODE_TFBS_ClusteredV3_CellTypes",
        n_top_sources = 1
    )
    ## n_top_sources=1 should keep only the most frequent source
    expect_equal(length(unique(result$Source)), 1)
})
