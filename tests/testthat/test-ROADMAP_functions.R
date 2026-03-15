test_that("ROADMAP_chromatin_states returns data.table", {
    d <- echoannot:::ROADMAP_chromatin_states()
    expect_true(data.table::is.data.table(d))
    expect_true("MNEMONIC" %in% colnames(d))
    expect_true("DESCRIPTION" %in% colnames(d))
    expect_true(nrow(d) > 0)
})

test_that("ROADMAP_chromatin_states as_dict returns named character", {
    d <- echoannot:::ROADMAP_chromatin_states(as_dict = TRUE)
    expect_true(is.character(d))
    expect_true(length(d) > 0)
    expect_true("Enh" %in% names(d))
    expect_equal(unname(d["Enh"]), "Enhancer")
    expect_equal(unname(d["EnhG"]), "Genic enhancer")
    expect_equal(unname(d["TssAFlnk"]), "Active flanking TSS")
    expect_equal(unname(d["BivFlnk"]), "Bivalent/Poised flanking TSS")
})

test_that("ROADMAP_construct_reference loads reference data", {
    ref <- echoannot::ROADMAP_construct_reference(verbose = FALSE)
    expect_true(data.table::is.data.table(ref))
    expect_true("EID" %in% colnames(ref))
    expect_true(nrow(ref) > 0)
})

test_that("ROADMAP_construct_reference keyword_query filters", {
    ref_all <- echoannot::ROADMAP_construct_reference(verbose = FALSE)
    ref_sub <- echoannot::ROADMAP_construct_reference(
        keyword_query = "brain",
        verbose = FALSE
    )
    expect_true(nrow(ref_sub) > 0)
    expect_true(nrow(ref_sub) < nrow(ref_all))
})

test_that("ROADMAP_construct_reference limit_files works", {
    ref <- echoannot::ROADMAP_construct_reference(
        limit_files = 3,
        verbose = FALSE
    )
    expect_equal(nrow(ref), 3)
})

test_that("ROADMAP_construct_reference keyword + limit combined", {
    ref <- echoannot::ROADMAP_construct_reference(
        keyword_query = "brain",
        limit_files = 2,
        verbose = FALSE
    )
    expect_equal(nrow(ref), 2)
})
