test_that("XGR_sep_handler returns correct separators", {
    expect_equal(
        echoannot:::XGR_sep_handler("ENCODE_TFBS_ClusteredV3_CellTypes"),
        "[.]"
    )
    expect_equal(
        echoannot:::XGR_sep_handler("ENCODE_DNaseI_ClusteredV3_CellTypes"),
        "_(?=[^_]+$)"
    )
    expect_equal(
        echoannot:::XGR_sep_handler("Broad_Histone"),
        "_(?=[^_]+$)"
    )
    expect_equal(
        echoannot:::XGR_sep_handler("FANTOM5_Enhancer"),
        "_(?=[^_]+$)"
    )
    expect_equal(
        echoannot:::XGR_sep_handler("TFBS_Conserved"),
        "[$]"
    )
})

test_that("XGR_sep_handler default for unknown library", {
    expect_equal(
        echoannot:::XGR_sep_handler("SomeUnknownLib"),
        "_(?=[^_]+$)"
    )
})

test_that("XGR_filter_assays works with xgr_example", {
    data("xgr_example", package = "echoannot")
    result <- echoannot::XGR_filter_assays(
        gr.lib = xgr_example,
        n_top_assays = 1
    )
    expect_true(methods::is(result, "GRanges"))
    expect_true(length(result) > 0)
    expect_true(length(result) <= length(xgr_example))
    assays_remaining <- unique(result$Assay)
    expect_true(length(assays_remaining) >= 1)
})

test_that("XGR_filter_assays with NULL keeps all", {
    data("xgr_example", package = "echoannot")
    result <- echoannot::XGR_filter_assays(
        gr.lib = xgr_example,
        n_top_assays = NULL
    )
    expect_equal(length(result), length(xgr_example))
})

test_that("XGR_filter_sources works with xgr_example", {
    data("xgr_example", package = "echoannot")
    result <- echoannot::XGR_filter_sources(
        gr.lib = xgr_example,
        n_top_sources = 1
    )
    expect_true(methods::is(result, "GRanges"))
    expect_true(length(result) > 0)
    expect_true(length(result) <= length(xgr_example))
})

test_that("XGR_filter_sources with NULL keeps all", {
    data("xgr_example", package = "echoannot")
    result <- echoannot::XGR_filter_sources(
        gr.lib = xgr_example,
        n_top_sources = NULL
    )
    expect_equal(length(result), length(xgr_example))
})

test_that("XGR_parse_metadata parses fullname into Source and Assay", {
    data("xgr_example", package = "echoannot")
    ## Create a small GRanges with a fullname column
    gr <- xgr_example[1:5]
    GenomicRanges::mcols(gr) <- data.frame(
        fullname = paste0("CellA_AssayX"),
        stringsAsFactors = FALSE
    )
    result <- echoannot:::XGR_parse_metadata(
        gr.lib = gr,
        lib.name = "ENCODE_DNaseI_ClusteredV3_CellTypes"
    )
    expect_true("Source" %in% colnames(GenomicRanges::mcols(result)))
    expect_true("Assay" %in% colnames(GenomicRanges::mcols(result)))
})
