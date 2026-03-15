test_that("messager prints when v=TRUE", {
    expect_message(
        echoannot:::messager("hello world", v = TRUE),
        "hello world"
    )
})

test_that("messager suppresses when v=FALSE", {
    expect_silent(
        echoannot:::messager("hello world", v = FALSE)
    )
})

test_that("messager concatenates multiple args", {
    expect_message(
        echoannot:::messager("foo", "bar", "baz", v = TRUE),
        "foo bar baz"
    )
})

test_that("message_parallel prints when v=TRUE", {
    out <- echoannot:::message_parallel("test message", v = TRUE)
    expect_equal(out, 0L)
})

test_that("message_parallel suppresses when v=FALSE", {
    expect_null(
        echoannot:::message_parallel("test message", v = FALSE)
    )
})

test_that("palette_gnbu returns 9 colors by default", {
    cols <- echoannot:::palette_gnbu()
    expect_length(cols, 9)
    expect_true(all(grepl("^#", cols)))
})

test_that("palette_gnbu respects n argument", {
    cols <- echoannot:::palette_gnbu(n = 3)
    expect_length(cols, 3)

    cols_large <- echoannot:::palette_gnbu(n = 100)
    expect_length(cols_large, 9)
})

test_that("assay_color_dict returns named vector", {
    d <- echoannot:::assay_color_dict()
    expect_true(is.character(d))
    expect_true(all(c("ATAC", "H3K27ac", "H3K4me3", "peaks") %in% names(d)))
    expect_equal(unname(d["ATAC"]), "magenta")
})

test_that("require_arg stops on missing arg", {
    expect_error(
        echoannot:::require_arg("missing_arg", c("a", "b")),
        "must be supplied"
    )
})

test_that("require_arg succeeds when arg present", {
    expect_silent(
        echoannot:::require_arg("a", c("a", "b"))
    )
})

test_that("NOTT2019_marker_key returns correct mapping", {
    mk <- echoannot:::NOTT2019_marker_key()
    expect_true(is.list(mk))
    expect_equal(mk$PU1, "microglia")
    expect_equal(mk$Olig2, "oligo")
    expect_equal(mk$NeuN, "neurons")
    expect_equal(mk$LHX2, "astrocytes")
    expect_length(mk, 4)
})

test_that("annotation_file_name creates directory and path", {
    tmp <- tempdir()
    result <- echoannot::annotation_file_name(
        locus_dir = tmp,
        lib_name = "test_lib"
    )
    expect_true(dir.exists(file.path(tmp, "annotations")))
    expect_true(grepl("test_lib\\.rds$", result))
    expect_true(grepl("annotations", result))
})

test_that("annotation_file_name uses custom suffix", {
    tmp <- tempdir()
    result <- echoannot::annotation_file_name(
        locus_dir = tmp,
        lib_name = "test_lib",
        suffix = ".tsv"
    )
    expect_true(grepl("test_lib\\.tsv$", result))
})
