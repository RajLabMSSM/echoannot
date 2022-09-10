test_that("IMPACT_query works", {
  
    query_dat <- echodata::BST1[1:50,]
    annot_dt <- IMPACT_query(query_dat=query_dat,
                             populations="EUR")
     testthat::expect_equal(dim(annot_dt),c(13,1419))
     
     #### Currently fails 
     ## (need to upgrade echotabix so it handles multiple chroms)
     # query_dat <- rbind(echodata::BST1[1:50,], 
     #                    echodata::LRRK2[1:50,], fill=TRUE)
     # annot_dt <- IMPACT_query(query_dat=query_dat,
     #                          populations="EUR")
     # testthat::expect_equal(dim(annot_dt),c(13,1419)) 
})
