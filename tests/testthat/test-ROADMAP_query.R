test_that("ROADMAP_query works", {
  
    query_dat <- echodata::BST1
    #### as file names ####
    bed_paths <- ROADMAP_query(
        query_dat = query_dat,
        keyword_query = "placenta", 
        return_paths = TRUE)
    testthat::expect_true(all(file.exists(unlist(bed_paths))))
    
    #### as granges ####
    grl.roadmap <- ROADMAP_query(
        query_dat = query_dat,
        keyword_query = "placenta", 
        return_paths = FALSE)
    testthat::expect_true(methods::is(grl.roadmap,"GRangesList"))
    testthat::expect_true(
        all(unlist(lapply(grl.roadmap, length))>c(290,180))
    )
})
