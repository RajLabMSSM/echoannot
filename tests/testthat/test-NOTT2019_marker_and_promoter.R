test_that("NOTT2019_get_promoter_celltypes identifies active promoters", {
    ## Create mock data resembling the NOTT2019 interactome data
    annot_sub <- data.frame(
        PU1_active_promoter = c(1, 0, 1),
        NeuN_active_promoter = c(0, 1, 0),
        Olig2_active_promoter = c(0, 0, 0),
        LHX2_active_promoter = c(1, 1, 0)
    )
    marker_key <- echoannot:::NOTT2019_marker_key()

    result <- echoannot:::NOTT2019_get_promoter_celltypes(
        annot_sub = annot_sub,
        marker_key = marker_key,
        verbose = FALSE
    )
    expect_true(is.character(result))
    ## PU1 -> microglia, NeuN -> neurons, LHX2 -> astrocytes are all active
    expect_true(grepl("microglia", result))
    expect_true(grepl("neurons", result))
    expect_true(grepl("astrocytes", result))
    ## Olig2 column sums to 0, so oligo should not appear
    expect_false(grepl("oligo", result))
})

test_that("NOTT2019_get_promoter_celltypes returns empty string when none active", {
    annot_sub <- data.frame(
        PU1_active_promoter = c(0, 0),
        NeuN_active_promoter = c(0, 0),
        Olig2_active_promoter = c(0, 0),
        LHX2_active_promoter = c(0, 0)
    )
    marker_key <- echoannot:::NOTT2019_marker_key()

    result <- echoannot:::NOTT2019_get_promoter_celltypes(
        annot_sub = annot_sub,
        marker_key = marker_key,
        verbose = FALSE
    )
    expect_true(is.character(result))
    expect_equal(result, "")
})

test_that("NOTT2019_bigwig_metadata is available as package data", {
    data("NOTT2019_bigwig_metadata", package = "echoannot")
    expect_true(exists("NOTT2019_bigwig_metadata"))
    expect_true(is.data.frame(NOTT2019_bigwig_metadata))
    expect_true(nrow(NOTT2019_bigwig_metadata) > 0)
    expect_true("name" %in% colnames(NOTT2019_bigwig_metadata) ||
                "data_link" %in% colnames(NOTT2019_bigwig_metadata))
})
