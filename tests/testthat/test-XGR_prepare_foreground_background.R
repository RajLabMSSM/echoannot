test_that("XGR_prepare_foreground_background returns fg and bg lists", {
    dat <- echodata::BST1
    result <- echoannot:::XGR_prepare_foreground_background(
        dat = dat,
        foreground_filter = "Consensus_SNP==TRUE",
        background_filter = NULL,
        verbose = FALSE
    )
    expect_true(is.list(result))
    expect_true("foreground" %in% names(result))
    expect_true("background" %in% names(result))
    expect_true(is.data.frame(result$foreground))
    expect_true(is.data.frame(result$background))
    expect_true(all(c("chrom", "chromStart", "chromEnd", "name") %in%
                        colnames(result$foreground)))
})

test_that("XGR_prepare_foreground_background foreground is subset", {
    dat <- echodata::BST1
    result <- echoannot:::XGR_prepare_foreground_background(
        dat = dat,
        foreground_filter = "Consensus_SNP==TRUE",
        background_filter = NULL,
        verbose = FALSE
    )
    n_consensus <- sum(dat$Consensus_SNP == TRUE, na.rm = TRUE)
    expect_equal(nrow(result$foreground), n_consensus)
    ## Background with NULL filter should include all SNPs
    expect_equal(nrow(result$background), nrow(dat))
})

test_that("XGR_prepare_foreground_background with NA background returns NULL", {
    dat <- echodata::BST1
    result <- echoannot:::XGR_prepare_foreground_background(
        dat = dat,
        foreground_filter = "Consensus_SNP==TRUE",
        background_filter = NA,
        verbose = FALSE
    )
    expect_null(result$background)
})

test_that("XGR_prepare_foreground_background with explicit bg filter", {
    dat <- echodata::BST1
    result <- echoannot:::XGR_prepare_foreground_background(
        dat = dat,
        foreground_filter = "Consensus_SNP==TRUE",
        background_filter = "leadSNP==TRUE",
        verbose = FALSE
    )
    n_lead <- sum(dat$leadSNP == TRUE, na.rm = TRUE)
    expect_equal(nrow(result$background), n_lead)
})

test_that("XGR_prepare_foreground_background sampling works", {
    dat <- echodata::BST1
    result <- echoannot:::XGR_prepare_foreground_background(
        dat = dat,
        foreground_filter = "Support>0",
        background_filter = NULL,
        fg_sample_size = 2,
        bg_sample_size = 5,
        verbose = FALSE
    )
    expect_equal(nrow(result$foreground), 2)
    expect_equal(nrow(result$background), 5)
})
