test_that("get_lead_pos returns leadSNP position", {
    dat <- echodata::BST1
    result <- echoannot:::get_lead_pos(
        dat = dat,
        xvar = "POS",
        index_SNP = NULL
    )
    lead_rows <- dat[dat$leadSNP == TRUE, ]
    expect_equal(result, lead_rows$POS)
})

test_that("get_lead_pos returns index_SNP position", {
    dat <- echodata::BST1
    snp <- dat$SNP[1]
    result <- echoannot:::get_lead_pos(
        dat = dat,
        xvar = "POS",
        index_SNP = snp
    )
    expect_equal(result, dat$POS[dat$SNP == snp])
})

test_that("get_consensus_pos returns consensus SNP positions", {
    dat <- echodata::BST1
    result <- echoannot:::get_consensus_pos(
        dat = dat,
        xvar = "POS"
    )
    consensus_rows <- dat[dat$Consensus_SNP == TRUE, ]
    expect_equal(result, consensus_rows$POS)
})

test_that("get_consensus_pos returns empty for no consensus", {
    dat <- data.table::copy(echodata::BST1[1:5, ])
    dat$Consensus_SNP <- FALSE
    result <- echoannot:::get_consensus_pos(
        dat = dat,
        xvar = "POS"
    )
    expect_length(result, 0)
})

test_that("get_zoom_xlims computes symmetric window", {
    result <- echoannot:::get_zoom_xlims(
        lead.pos = 1000000,
        zoom_window = 50000,
        verbose = FALSE
    )
    expect_length(result, 2)
    expect_equal(result[1], 975000)
    expect_equal(result[2], 1025000)
})

test_that("get_zoom_xlims handles large window", {
    result <- echoannot:::get_zoom_xlims(
        lead.pos = 500000,
        zoom_window = 200000,
        verbose = FALSE
    )
    expect_equal(result[2] - result[1], 200000)
})

test_that("check_bigwig_metadata passes with valid columns", {
    bw <- data.frame(name = "test", data_link = "http://example.com")
    expect_null(echoannot:::check_bigwig_metadata(bw))
})

test_that("check_bigwig_metadata errors on missing columns", {
    bw <- data.frame(name = "test")
    expect_error(
        echoannot:::check_bigwig_metadata(bw),
        "Missing required columns"
    )
})

test_that("check_bigwig_metadata custom required_cols", {
    bw <- data.frame(col_a = 1, col_b = 2)
    expect_null(
        echoannot:::check_bigwig_metadata(
            bw,
            required_cols = c("col_a", "col_b")
        )
    )
    expect_error(
        echoannot:::check_bigwig_metadata(
            bw,
            required_cols = c("col_a", "col_c")
        ),
        "Missing required columns"
    )
})

test_that("save_annotations saves RDS file", {
    tmp <- tempfile(fileext = ".rds")
    gr <- GenomicRanges::GRanges(
        seqnames = "chr1",
        ranges = IRanges::IRanges(start = 100, end = 200)
    )
    echoannot:::save_annotations(
        gr = gr,
        anno_path = tmp,
        libName = "test",
        verbose = FALSE
    )
    expect_true(file.exists(tmp))
    loaded <- readRDS(tmp)
    expect_true(methods::is(loaded, "GRanges"))
    unlink(tmp)
})
