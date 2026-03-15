test_that("ROADMAP_annotate adds Source column to GRanges list", {
    ## Create a simple GRanges list with EID metadata
    gr1 <- GenomicRanges::GRanges(
        seqnames = c("chr1", "chr1"),
        ranges = IRanges::IRanges(start = c(100, 200), end = c(150, 250))
    )
    GenomicRanges::mcols(gr1)$EID <- "E003"
    GenomicRanges::mcols(gr1)$name <- "test"

    grlist <- list(item1 = gr1)

    ## Get the ROADMAP reference to find a valid EID
    ref <- echoannot::ROADMAP_construct_reference(verbose = FALSE)

    result <- echoannot:::ROADMAP_annotate(
        grlist = grlist,
        ref = ref,
        verbose = FALSE
    )
    expect_true(is.list(result))
    expect_true(length(result) == 1)
    gr_out <- result[[1]]
    expect_true("Source" %in% colnames(GenomicRanges::mcols(gr_out)))
})

test_that("ROADMAP_annotate uses keyword_query when ref is NULL", {
    gr1 <- GenomicRanges::GRanges(
        seqnames = "chr1",
        ranges = IRanges::IRanges(start = 100, end = 200)
    )
    ## Use a real EID that exists in the reference
    ref <- echoannot::ROADMAP_construct_reference(verbose = FALSE)
    real_eid <- ref$EID[1]
    GenomicRanges::mcols(gr1)$EID <- real_eid
    GenomicRanges::mcols(gr1)$name <- "test"

    grlist <- list(item1 = gr1)
    result <- echoannot:::ROADMAP_annotate(
        grlist = grlist,
        ref = NULL,
        keyword_query = NULL,
        verbose = FALSE
    )
    expect_true(is.list(result))
    gr_out <- result[[1]]
    expect_true("Source" %in% colnames(GenomicRanges::mcols(gr_out)))
})
