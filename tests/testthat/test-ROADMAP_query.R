test_that("ROADMAP_query works", {
  
    query_dat <- echodata::BST1
    #### as file names ####
    bed_paths <- ROADMAP_query(
        query_dat = query_dat,
        keyword_query = "placenta", 
        limit_files = 1,
        return_paths = TRUE)
    testthat::expect_true(all(file.exists(unlist(bed_paths))))
    
    #### as granges ####
    grl1 <- ROADMAP_query(
        query_dat = query_dat,
        keyword_query = "placenta", 
        limit_files = 1,
        return_paths = FALSE) 
    testthat::expect_true(methods::is(grl1,"GRangesList"))
    testthat::expect_true(
        all(unlist(lapply(grl1, length))>c(290,180))
    )
    #### from pre-existing file, and with merge_and_process=TRUE ####
    grl2 <- ROADMAP_query(
        query_dat = query_dat,
        keyword_query = "placenta",
        limit_files = 1,
        merge_and_process = TRUE,
        return_paths = FALSE)
    testthat::expect_true(methods::is(grl2,"GRanges"))
    testthat::expect_length(grl2,258)
})
