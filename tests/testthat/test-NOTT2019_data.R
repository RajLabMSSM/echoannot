test_that("NOTT2019_bigwig_metadata loads correctly", {
    data("NOTT2019_bigwig_metadata", package = "echoannot")
    expect_true(data.table::is.data.table(NOTT2019_bigwig_metadata))
    expect_true(nrow(NOTT2019_bigwig_metadata) > 0)
    expect_true("name" %in% colnames(NOTT2019_bigwig_metadata) ||
                "data_link" %in% colnames(NOTT2019_bigwig_metadata))
})

test_that("NOTT2019_marker_key covers expected markers", {
    mk <- echoannot:::NOTT2019_marker_key()
    expect_true(all(c("PU1", "Olig2", "NeuN", "LHX2") %in% names(mk)))
})

test_that("NOTT2019_get_promoter_celltypes returns cell type string", {
    ## Create mock data matching expected structure
    annot_sub <- data.frame(
        PU1_active_promoter = c(0, 1, 1),
        NeuN_active_promoter = c(1, 0, 0),
        Olig2_active_promoter = c(0, 0, 0),
        LHX2_active_promoter = c(0, 0, 0)
    )
    marker_key <- echoannot:::NOTT2019_marker_key()

    result <- echoannot:::NOTT2019_get_promoter_celltypes(
        annot_sub = annot_sub,
        marker_key = marker_key,
        verbose = FALSE
    )
    expect_true(is.character(result))
    expect_true(nchar(result) > 0)
    expect_true(grepl("microglia", result))
    expect_true(grepl("neurons", result))
})

test_that("NOTT2019_get_promoter_celltypes empty when no active promoters", {
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
