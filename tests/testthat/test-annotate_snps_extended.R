test_that("annotate_snps errors when SNP_col is missing", {
    dat <- data.table::data.table(
        CHR = 4,
        POS = 15737348,
        NOT_SNP = "rs123"
    )
    expect_error(
        echoannot::annotate_snps(
            dat = dat,
            SNP_col = "SNP",
            haploreg_annotation = FALSE,
            regulomeDB_annotation = FALSE,
            biomart_annotation = FALSE
        ),
        "SNP_col must be present in dat"
    )
})

test_that("annotate_snps returns data.table with no annotations", {
    dat <- echodata::BST1[1:5, ]
    result <- echoannot::annotate_snps(
        dat = dat,
        haploreg_annotation = FALSE,
        regulomeDB_annotation = FALSE,
        biomart_annotation = FALSE,
        verbose = FALSE
    )
    expect_true(data.table::is.data.table(result))
    expect_equal(nrow(result), nrow(dat))
    expect_true("SNP" %in% colnames(result))
})

test_that("annotate_snps accepts custom SNP_col", {
    dat <- data.table::data.table(
        mysnp = c("rs123", "rs456"),
        CHR = c(1, 2),
        POS = c(1000, 2000)
    )
    result <- echoannot::annotate_snps(
        dat = dat,
        SNP_col = "mysnp",
        haploreg_annotation = FALSE,
        regulomeDB_annotation = FALSE,
        biomart_annotation = FALSE,
        verbose = FALSE
    )
    expect_true(data.table::is.data.table(result))
    expect_equal(nrow(result), 2)
})

test_that("annotate_snps converts input to data.table", {
    dat <- as.data.frame(echodata::BST1[1:3, ])
    result <- echoannot::annotate_snps(
        dat = dat,
        haploreg_annotation = FALSE,
        regulomeDB_annotation = FALSE,
        biomart_annotation = FALSE,
        verbose = FALSE
    )
    expect_true(data.table::is.data.table(result))
})
