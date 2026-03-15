test_that("standardize_celltypes collapses subtypes by default", {
    input <- c("ExcitatoryNeurons", "InhibitoryNeurons", "NigralNeurons")
    result <- echoannot:::standardize_celltypes(input)
    expect_true(all(result == "neurons"))
})

test_that("standardize_celltypes preserves basic types", {
    input <- c("astrocytes", "microglia", "oligo", "neurons", "brain")
    result <- echoannot:::standardize_celltypes(input)
    expect_equal(unname(result), input)
})

test_that("standardize_celltypes maps title-case names", {
    input <- c("Microglia", "Oligodendrocytes", "Astrocytes")
    result <- echoannot:::standardize_celltypes(input)
    expect_equal(unname(result), c("microglia", "oligo", "astrocytes"))
})

test_that("standardize_celltypes OPCs map to oligo without subtypes", {
    result <- echoannot:::standardize_celltypes("OPCs", allow_subtypes = FALSE)
    expect_equal(unname(result), "oligo")
})

test_that("standardize_celltypes allows subtypes when requested", {
    input <- c("ExcitatoryNeurons", "InhibitoryNeurons", "NigralNeurons")
    result <- echoannot:::standardize_celltypes(input, allow_subtypes = TRUE)
    expect_equal(unname(result), c("neurons (+)", "neurons (-)", "neurons (nigral)"))
})

test_that("standardize_celltypes OPCs stay as OPCs with subtypes", {
    result <- echoannot:::standardize_celltypes("OPCs", allow_subtypes = TRUE)
    expect_equal(unname(result), "OPCs")
})
