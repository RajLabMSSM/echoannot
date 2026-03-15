test_that("save_annotations saves RDS file", {
    tmp <- tempfile(fileext = ".rds")
    gr <- GenomicRanges::GRanges(
        seqnames = "chr1",
        ranges = IRanges::IRanges(start = 100, end = 200)
    )
    echoannot:::save_annotations(
        gr = gr,
        anno_path = tmp,
        libName = "test_lib",
        verbose = FALSE
    )
    expect_true(file.exists(tmp))
    loaded <- readRDS(tmp)
    expect_true(methods::is(loaded, "GRanges"))
    unlink(tmp)
})

test_that("save_annotations creates directory if needed", {
    tmp_dir <- file.path(tempdir(), "test_save_annot_subdir")
    tmp <- file.path(tmp_dir, "test.rds")
    if (dir.exists(tmp_dir)) unlink(tmp_dir, recursive = TRUE)

    gr <- GenomicRanges::GRanges(
        seqnames = "chr1",
        ranges = IRanges::IRanges(start = 100, end = 200)
    )
    echoannot:::save_annotations(
        gr = gr,
        anno_path = tmp,
        libName = "test_lib",
        verbose = FALSE
    )
    expect_true(dir.exists(tmp_dir))
    expect_true(file.exists(tmp))
    unlink(tmp_dir, recursive = TRUE)
})

test_that("save_annotations prints message when verbose", {
    tmp <- tempfile(fileext = ".rds")
    gr <- GenomicRanges::GRanges(
        seqnames = "chr1",
        ranges = IRanges::IRanges(start = 100, end = 200)
    )
    expect_message(
        echoannot:::save_annotations(
            gr = gr,
            anno_path = tmp,
            libName = "test_lib",
            verbose = TRUE
        ),
        "Saving annotations"
    )
    unlink(tmp)
})
