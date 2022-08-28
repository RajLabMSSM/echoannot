test_that("annotate_finemapping_results works", {
   
    dat <- echodata::BST1[Consensus_SNP==TRUE,]
    dat_annot <- annotate_snps(dat = dat,
                               haploreg_annotation = TRUE,
                               regulomeDB_annotation = TRUE,
                               biomart_annotation = TRUE)
    testthat::expect_gte(ncol(dat),20)
    testthat::expect_gte(ncol(dat_annot),80)
    testthat::expect_true("Promoter_histone_marks" %in% colnames(dat_annot))
})
