test_that("get_CORCES2020_scATACseq_celltype_peaks returns data.table", {
    skip_if_offline()
    dat <- echoannot::get_CORCES2020_scATACseq_celltype_peaks()
    expect_true(data.table::is.data.table(dat))
    expect_true(nrow(dat) > 0)
    expect_true("hg38_Chromosome" %in% colnames(dat))
    expect_true("hg38_Start" %in% colnames(dat))
    expect_true("hg38_Stop" %in% colnames(dat))
})

test_that("get_CORCES2020_bulkATACseq_peaks returns data.table", {
    skip_if_offline()
    dat <- echoannot::get_CORCES2020_bulkATACseq_peaks()
    expect_true(data.table::is.data.table(dat))
    expect_true(nrow(dat) > 0)
    expect_true("hg38_Chromosome" %in% colnames(dat))
})

test_that("get_CORCES2020_scATACseq_peaks returns data.table", {
    skip_if_offline()
    dat <- echoannot::get_CORCES2020_scATACseq_peaks()
    expect_true(data.table::is.data.table(dat))
    expect_true(nrow(dat) > 0)
})

test_that("get_CORCES2020_cicero_coaccessibility returns data.table", {
    skip_if_offline()
    dat <- echoannot::get_CORCES2020_cicero_coaccessibility()
    expect_true(data.table::is.data.table(dat))
    expect_true(nrow(dat) > 0)
    expect_true("Coaccessibility" %in% colnames(dat))
})

test_that("get_CORCES2020_hichip_fithichip_loop_calls returns data.table", {
    skip_if_offline()
    dat <- echoannot::get_CORCES2020_hichip_fithichip_loop_calls()
    expect_true(data.table::is.data.table(dat))
    expect_true(nrow(dat) > 0)
})
