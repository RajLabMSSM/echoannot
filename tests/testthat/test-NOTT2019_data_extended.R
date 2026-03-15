test_that("get_NOTT2019_interactome returns named list", {
    skip_if_offline()
    dat <- echoannot:::get_NOTT2019_interactome()
    expect_true(is.list(dat))
    expect_true(length(dat) > 0)
    expect_true(!is.null(names(dat)))
    ## Should contain sheets with promoter/enhancer/interactome
    has_promoter <- any(grepl("promoter", names(dat), ignore.case = TRUE))
    has_enhancer <- any(grepl("enhancer", names(dat), ignore.case = TRUE))
    has_interactome <- any(grepl("interactome", names(dat), ignore.case = TRUE))
    expect_true(has_promoter || has_enhancer || has_interactome)
})

test_that("get_NOTT2019_superenhancer_interactome returns data.table", {
    skip_if_offline()
    dat <- echoannot::get_NOTT2019_superenhancer_interactome()
    expect_true(data.table::is.data.table(dat))
    expect_true(nrow(dat) > 0)
    expect_true("chr" %in% colnames(dat))
})

test_that("NOTT2019_get_regulatory_regions returns data.frame", {
    skip_if_offline()
    regions <- echoannot::NOTT2019_get_regulatory_regions(
        as_granges = FALSE,
        verbose = FALSE
    )
    expect_true(is.data.frame(regions))
    expect_true(nrow(regions) > 0)
    expect_true("Cell_type" %in% colnames(regions))
    expect_true("Element" %in% colnames(regions))
    ## Cell_type should be standardized
    valid_celltypes <- c("astrocytes", "neurons", "oligo", "microglia")
    expect_true(all(regions$Cell_type %in% valid_celltypes))
})

test_that("NOTT2019_get_regulatory_regions as_granges returns GRanges", {
    skip_if_offline()
    regions <- echoannot::NOTT2019_get_regulatory_regions(
        as_granges = TRUE,
        verbose = FALSE
    )
    expect_true(methods::is(regions, "GRanges"))
    expect_true(length(regions) > 0)
})
