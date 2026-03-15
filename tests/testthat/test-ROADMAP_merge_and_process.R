test_that("ROADMAP_merge_and_process merges and filters", {
    ## Create a GRangesList with Source metadata
    gr1 <- GenomicRanges::GRanges(
        seqnames = c("chr4", "chr4", "chr4"),
        ranges = IRanges::IRanges(
            start = c(15723865, 15737348, 15750000),
            end = c(15724000, 15737500, 15751000)
        )
    )
    GenomicRanges::mcols(gr1)$name <- c("1_TssA", "2_Enh", "3_Quies")
    GenomicRanges::mcols(gr1)$EID <- "E001"
    GenomicRanges::mcols(gr1)$Source <- "Brain_Tissue"

    grl <- GenomicRanges::GRangesList(test = gr1)

    ## Use BST1 as the query
    gr_snp <- echodata::BST1

    result <- echoannot:::ROADMAP_merge_and_process(
        grl.roadmap = grl,
        gr.snp = gr_snp,
        n_top = NULL,
        verbose = FALSE
    )
    expect_true(methods::is(result, "GRanges"))
})

test_that("ROADMAP_merge_and_process n_top limits sources", {
    ## Create GRanges with multiple sources
    gr1 <- GenomicRanges::GRanges(
        seqnames = rep("chr4", 5),
        ranges = IRanges::IRanges(
            start = seq(15723865, 15724265, 100),
            end = seq(15724865, 15725265, 100)
        )
    )
    GenomicRanges::mcols(gr1)$Source <- "SourceA"
    GenomicRanges::mcols(gr1)$name <- paste0("1_TssA")

    gr2 <- GenomicRanges::GRanges(
        seqnames = rep("chr4", 2),
        ranges = IRanges::IRanges(
            start = c(15723865, 15723965),
            end = c(15724865, 15724965)
        )
    )
    GenomicRanges::mcols(gr2)$Source <- "SourceB"
    GenomicRanges::mcols(gr2)$name <- paste0("2_Enh")

    grl <- GenomicRanges::GRangesList(a = gr1, b = gr2)
    gr_snp <- echodata::BST1

    result <- echoannot:::ROADMAP_merge_and_process(
        grl.roadmap = grl,
        gr.snp = gr_snp,
        n_top = 1,
        verbose = FALSE
    )
    expect_true(methods::is(result, "GRanges"))
    ## Should only keep the top source
    expect_true(length(unique(result$Source)) <= 1)
})
