test_that("select_genome errors on invalid genome build", {
    expect_error(
        echoannot:::select_genome(genome_build = "hg99", verbose = FALSE),
        "genome_build must be one of"
    )
})

test_that("select_genome defaults to hg19 when NULL", {
    skip_if_not_installed("SNPlocs.Hsapiens.dbSNP144.GRCh37")
    skip_if_not_installed("BSgenome.Hsapiens.UCSC.hg19")
    result <- echoannot:::select_genome(genome_build = NULL, verbose = FALSE)
    expect_equal(result$genome_build, "hg19")
    expect_true(is.list(result))
    expect_true("dbSNP" %in% names(result))
    expect_true("search.genome" %in% names(result))
})

test_that("select_genome normalizes case", {
    expect_error(
        echoannot:::select_genome(genome_build = "HG99"),
        "genome_build must be one of"
    )
})

test_that("select_genome accepts hg19", {
    skip_if_not_installed("SNPlocs.Hsapiens.dbSNP144.GRCh37")
    skip_if_not_installed("BSgenome.Hsapiens.UCSC.hg19")
    result <- echoannot:::select_genome(genome_build = "hg19", verbose = FALSE)
    expect_equal(result$genome_build, "hg19")
})
