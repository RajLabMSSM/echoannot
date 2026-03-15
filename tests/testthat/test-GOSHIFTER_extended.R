test_that("GOSHIFTER_search_ROADMAP filters by EID", {
    ref_path <- system.file(
        "extdata/ROADMAP",
        "ROADMAP_Epigenomic.js",
        package = "echoannot"
    )
    ## Get all entries first to find a valid EID
    all_ref <- echoannot:::GOSHIFTER_search_ROADMAP(
        Roadmap_reference = ref_path,
        verbose = FALSE
    )
    valid_eid <- all_ref$EID[1]

    result <- echoannot:::GOSHIFTER_search_ROADMAP(
        Roadmap_reference = ref_path,
        EID_filter = valid_eid,
        verbose = FALSE
    )
    expect_true(nrow(result) >= 1)
    expect_true(all(result$EID == valid_eid))
})

test_that("GOSHIFTER_search_ROADMAP filters by GROUP", {
    ref_path <- system.file(
        "extdata/ROADMAP",
        "ROADMAP_Epigenomic.js",
        package = "echoannot"
    )
    all_ref <- echoannot:::GOSHIFTER_search_ROADMAP(
        Roadmap_reference = ref_path,
        verbose = FALSE
    )
    valid_group <- all_ref$GROUP[1]

    result <- echoannot:::GOSHIFTER_search_ROADMAP(
        Roadmap_reference = ref_path,
        GROUP_filter = valid_group,
        verbose = FALSE
    )
    expect_true(nrow(result) >= 1)
    expect_true(all(result$GROUP == valid_group))
})

test_that("GOSHIFTER_find_folder errors when NULL and echolocatoR not installed", {
    ## The NULL path relies on echolocatoR being installed
    ## If echolocatoR is not installed, this should error
    skip_if(
        nzchar(system.file("tools/goshifter", package = "echolocatoR")),
        "echolocatoR has GoShifter bundled - skip this test"
    )
    expect_error(
        echoannot:::GOSHIFTER_find_folder(goshifter = NULL),
        "GoShifter directory not found"
    )
})

test_that("GOSHIFTER_list_chromatin_states uses custom annotations_path", {
    ## Test with the default package path explicitly
    result <- echoannot:::GOSHIFTER_list_chromatin_states(
        annotations_path = NULL
    )
    expect_true(data.table::is.data.table(result))
    expect_true(nrow(result) > 0)
})

test_that("GOSHIFTER_bed_names with single EID", {
    ref <- data.table::data.table(EID = "E099")
    result <- echoannot:::GOSHIFTER_bed_names(RM_ref = ref)
    expect_length(result, 1)
    expect_true(grepl("^E099", result))
    expect_true(grepl("\\.bed\\.gz$", result))
})
