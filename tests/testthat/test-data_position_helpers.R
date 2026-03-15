test_that("get_lead_pos returns lead SNP position by leadSNP column", {
    dat <- echodata::BST1
    pos <- echoannot:::get_lead_pos(dat, xvar = "POS", index_SNP = NULL)
    expect_true(length(pos) >= 1)
    expect_true(is.numeric(pos))
    ## Should match the position of the leadSNP == TRUE row(s)
    lead_rows <- dat[dat$leadSNP == TRUE, ]
    expect_equal(pos, lead_rows$POS)
})

test_that("get_lead_pos returns position by index_SNP", {
    dat <- echodata::BST1
    snp <- dat$SNP[1]
    pos <- echoannot:::get_lead_pos(dat, xvar = "POS", index_SNP = snp)
    expect_equal(pos, dat$POS[dat$SNP == snp])
})

test_that("get_consensus_pos returns consensus SNP positions", {
    dat <- echodata::BST1
    cpos <- echoannot:::get_consensus_pos(dat, xvar = "POS")
    cs_rows <- dat[dat$Consensus_SNP == TRUE, ]
    expect_equal(cpos, cs_rows$POS)
})

test_that("get_consensus_pos returns empty when no consensus SNPs", {
    dat <- echodata::BST1
    dat$Consensus_SNP <- FALSE
    cpos <- echoannot:::get_consensus_pos(dat, xvar = "POS")
    expect_length(cpos, 0)
})

test_that("get_top_consensus_pos returns top fine-mapped position", {
    dat <- echodata::BST1
    top_pos <- echoannot:::get_top_consensus_pos(dat, xvar = "POS")
    expect_true(is.numeric(top_pos))
    expect_length(top_pos, 1)
    ## The top consensus pos should be one of the consensus SNP positions
    cs_rows <- dat[dat$Consensus_SNP == TRUE, ]
    expect_true(top_pos %in% cs_rows$POS)
})

test_that("get_zoom_xlims computes symmetric window", {
    lead_pos <- 1000000
    zoom <- 50000
    xlims <- echoannot:::get_zoom_xlims(
        lead.pos = lead_pos,
        zoom_window = zoom,
        verbose = FALSE
    )
    expect_length(xlims, 2)
    expect_equal(xlims[1], lead_pos - 25000L)
    expect_equal(xlims[2], lead_pos + 25000L)
})

test_that("get_zoom_xlims with small window", {
    xlims <- echoannot:::get_zoom_xlims(
        lead.pos = 500,
        zoom_window = 100,
        verbose = FALSE
    )
    expect_equal(xlims, c(450L, 550L))
})
