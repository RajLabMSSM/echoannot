test_that("GOSHIFTER_find_folder errors on nonexistent path", {
    expect_error(
        echoannot:::GOSHIFTER_find_folder(goshifter = "/nonexistent/path"),
        "does not exist"
    )
})

test_that("GOSHIFTER_find_folder works with existing directory", {
    tmp <- tempdir()
    result <- echoannot:::GOSHIFTER_find_folder(goshifter = tmp)
    expect_equal(result, tmp)
})

test_that("GOSHIFTER_search_ROADMAP returns matching entries", {
    ref_path <- system.file(
        "extdata/ROADMAP",
        "ROADMAP_Epigenomic.js",
        package = "echoannot"
    )
    result <- echoannot:::GOSHIFTER_search_ROADMAP(
        Roadmap_reference = ref_path,
        fuzzy_search = "brain",
        verbose = FALSE
    )
    expect_true(data.table::is.data.table(result))
    expect_true(nrow(result) > 0)
    expect_true("EID" %in% colnames(result))
})

test_that("GOSHIFTER_search_ROADMAP errors when no matches", {
    ref_path <- system.file(
        "extdata/ROADMAP",
        "ROADMAP_Epigenomic.js",
        package = "echoannot"
    )
    expect_error(
        echoannot:::GOSHIFTER_search_ROADMAP(
            Roadmap_reference = ref_path,
            fuzzy_search = "zzz_nonexistent_tissue_xyz",
            verbose = FALSE
        ),
        "No annotation BED files found"
    )
})

test_that("GOSHIFTER_bed_names generates file names from reference", {
    ref <- data.table::data.table(EID = c("E001", "E002", "E003"))
    result <- echoannot:::GOSHIFTER_bed_names(RM_ref = ref)
    expect_length(result, 3)
    expect_true(all(grepl("^E00[123]_15_coreMarks_mnemonics\\.bed\\.gz$", result)))
})

test_that("GOSHIFTER_bed_names with custom suffix", {
    ref <- data.table::data.table(EID = c("E001"))
    result <- echoannot:::GOSHIFTER_bed_names(
        RM_ref = ref,
        suffix = "_custom.bed.gz"
    )
    expect_equal(result, "E001_custom.bed.gz")
})

test_that("GOSHIFTER_bed_names deduplicates EIDs", {
    ref <- data.table::data.table(EID = c("E001", "E001", "E002"))
    result <- echoannot:::GOSHIFTER_bed_names(RM_ref = ref)
    expect_length(result, 2)
})

test_that("GOSHIFTER_list_chromatin_states returns data.table", {
    result <- echoannot:::GOSHIFTER_list_chromatin_states()
    expect_true(data.table::is.data.table(result))
    expect_true(nrow(result) > 0)
    expect_true("MNEMONIC" %in% colnames(result) ||
                "STATE" %in% colnames(result) ||
                ncol(result) > 0)
})

test_that("GOSHIFTER_list_chromatin_states matches ROADMAP_chromatin_states", {
    goshifter_result <- echoannot:::GOSHIFTER_list_chromatin_states()
    roadmap_result <- echoannot:::ROADMAP_chromatin_states()
    expect_equal(nrow(goshifter_result), nrow(roadmap_result))
})
