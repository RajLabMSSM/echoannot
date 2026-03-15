test_that("standardize_celltypes maps correctly without subtypes", {
    input <- c("astrocytes", "microglia", "oligo", "neurons", "OPCs",
               "ExcitatoryNeurons", "InhibitoryNeurons", "NigralNeurons",
               "Microglia", "Oligodendrocytes", "Astrocytes")
    result <- echoannot:::standardize_celltypes(
        celltype_vector = input,
        allow_subtypes = FALSE
    )
    ## OPCs should map to oligo
    expect_equal(unname(result[5]), "oligo")
    ## All neuron subtypes should map to "neurons"
    expect_equal(unname(result[6]), "neurons")  # ExcitatoryNeurons
    expect_equal(unname(result[7]), "neurons")  # InhibitoryNeurons
    expect_equal(unname(result[8]), "neurons")  # NigralNeurons
    ## Capitalized names
    expect_equal(unname(result[9]), "microglia")
    expect_equal(unname(result[10]), "oligo")
    expect_equal(unname(result[11]), "astrocytes")
})

test_that("standardize_celltypes with subtypes preserves neuron types", {
    input <- c("ExcitatoryNeurons", "InhibitoryNeurons", "NigralNeurons")
    result <- echoannot:::standardize_celltypes(
        celltype_vector = input,
        allow_subtypes = TRUE
    )
    expect_equal(unname(result[1]), "neurons (+)")
    expect_equal(unname(result[2]), "neurons (-)")
    expect_equal(unname(result[3]), "neurons (nigral)")
})

test_that("standardize_celltypes with subtypes maps OPCs separately", {
    result <- echoannot:::standardize_celltypes(
        celltype_vector = "OPCs",
        allow_subtypes = TRUE
    )
    expect_equal(unname(result), "OPCs")

    result_no_sub <- echoannot:::standardize_celltypes(
        celltype_vector = "OPCs",
        allow_subtypes = FALSE
    )
    expect_equal(unname(result_no_sub), "oligo")
})

test_that("standardize_celltypes returns NA for unknown cell types", {
    result <- echoannot:::standardize_celltypes(
        celltype_vector = "unknown_cell_type",
        allow_subtypes = FALSE
    )
    expect_true(is.na(result))
})

test_that("standardize_celltypes preserves brain type", {
    result <- echoannot:::standardize_celltypes(
        celltype_vector = "brain",
        allow_subtypes = FALSE
    )
    expect_equal(unname(result), "brain")
})
