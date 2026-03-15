test_that("order_loci alphabetically ascending", {
    dat <- data.table::data.table(
        Locus = c("BST1", "LRRK2", "AAA", "ZZZ"),
        POS = c(1, 2, 3, 4)
    )
    merged_DT <- dat
    result <- echoannot:::order_loci(
        dat = dat,
        merged_DT = merged_DT,
        by_UCS_size = FALSE,
        descending = FALSE,
        verbose = FALSE
    )
    expect_true(is.factor(result$Locus))
    expect_true(is.ordered(result$Locus))
    ## Ascending alphabetical: AAA, BST1, LRRK2, ZZZ
    expect_equal(levels(result$Locus), c("AAA", "BST1", "LRRK2", "ZZZ"))
})

test_that("order_loci alphabetically descending", {
    dat <- data.table::data.table(
        Locus = c("BST1", "LRRK2", "AAA"),
        POS = c(1, 2, 3)
    )
    merged_DT <- dat
    result <- echoannot:::order_loci(
        dat = dat,
        merged_DT = merged_DT,
        by_UCS_size = FALSE,
        descending = TRUE,
        verbose = FALSE
    )
    expect_equal(levels(result$Locus), c("LRRK2", "BST1", "AAA"))
})

test_that("order_loci preserves data", {
    dat <- data.table::data.table(
        Locus = c("BST1", "LRRK2"),
        POS = c(100, 200)
    )
    result <- echoannot:::order_loci(
        dat = dat,
        merged_DT = dat,
        descending = FALSE,
        verbose = FALSE
    )
    expect_equal(nrow(result), 2)
    expect_equal(result$POS, c(100, 200))
})
