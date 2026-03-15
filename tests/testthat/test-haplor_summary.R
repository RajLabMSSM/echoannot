test_that("haplor_epigenetics_summary computes summary stats", {
    ## Create mock HaploReg-like results
    merged_results <- data.table::data.table(
        rsID = paste0("rs", 1:10),
        Promoter_histone_marks = c(
            "BRN, BLD", "BRN", "", "BLD", "",
            "BRN, LNG", "", "BRN", "", "BLD"
        ),
        Enhancer_histone_marks = c(
            "BRN", "", "BLD, BRN", "", "LNG",
            "", "BRN", "", "BRN, BLD", ""
        )
    )
    result <- suppressMessages(
        echoannot:::haplor_epigenetics_summary(
            merged_results = merged_results,
            tissue_list = c("BRN"),
            epigenetic_variables = c(
                "Promoter_histone_marks",
                "Enhancer_histone_marks"
            )
        )
    )
    expect_true(is.matrix(result))
    expect_equal(nrow(result), 2)
    expect_true("Hits" %in% colnames(result))
    expect_true("Total_SNPs" %in% colnames(result))
    expect_true("Percent_Total" %in% colnames(result))
})

test_that("haplor_epigenetics_summary with multiple tissues", {
    merged_results <- data.table::data.table(
        rsID = paste0("rs", 1:5),
        Promoter_histone_marks = c("BRN", "BLD", "BRN, BLD", "", "LNG"),
        Enhancer_histone_marks = c("", "BRN", "", "BLD", "")
    )
    result <- suppressMessages(
        echoannot:::haplor_epigenetics_summary(
            merged_results = merged_results,
            tissue_list = c("BRN", "BLD")
        )
    )
    ## "BRN, BLD" matches both
    expect_true(result["Promoter_histone_marks", "Hits"] >= 3)
})
