test_that("find_top_consensus adds topConsensus column", {
    dat <- echodata::BST1
    result <- echoannot:::find_top_consensus(dat, n_top = 1)
    expect_true("topConsensus" %in% colnames(result))
    expect_true(is.logical(result$topConsensus))
})

test_that("find_top_consensus marks at least one SNP as top", {
    dat <- echodata::BST1
    result <- echoannot:::find_top_consensus(dat, n_top = 1)
    expect_true(any(result$topConsensus))
    ## The topConsensus SNP should also be a Consensus SNP
    top_snps <- result[result$topConsensus, ]
    expect_true(all(top_snps$Consensus_SNP))
})

test_that("find_top_consensus with n_top=2 may return more", {
    dat <- echodata::BST1
    result1 <- echoannot:::find_top_consensus(dat, n_top = 1)
    result2 <- echoannot:::find_top_consensus(dat, n_top = 2)
    expect_true(sum(result2$topConsensus) >= sum(result1$topConsensus))
})

test_that("find_top_consensus returns same nrow", {
    dat <- echodata::BST1
    result <- echoannot:::find_top_consensus(dat, n_top = 1)
    expect_equal(nrow(result), nrow(dat))
})
