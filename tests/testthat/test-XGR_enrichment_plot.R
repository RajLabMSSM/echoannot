test_that("XGR_enrichment_plot bar type returns ggplot", {
    enrich_res <- data.table::data.table(
        SNP_group = rep(c("Consensus", "Lead", "Random"), each = 3),
        fc = c(2.1, 1.5, 0.8, 1.2, 0.9, 1.1, 0.5, 0.6, 0.7),
        FDR = c(0.01, 0.03, 0.1, 0.02, 0.05, 0.2, 0.3, 0.4, 0.5),
        pvalue = c(0.001, 0.01, 0.05, 0.005, 0.02, 0.1, 0.15, 0.2, 0.3),
        nOverlap = c(10, 8, 5, 7, 6, 4, 3, 2, 1),
        Cell_type = rep(c("neurons", "microglia", "oligo"), 3),
        Assay = rep("ATAC", 9)
    )
    result <- echoannot:::XGR_enrichment_plot(
        enrich_res = enrich_res,
        plot_type = "bar",
        show_plot = FALSE,
        save_plot = FALSE
    )
    expect_true(methods::is(result, "ggplot"))
})

test_that("XGR_enrichment_plot point type returns ggplot", {
    enrich_res <- data.table::data.table(
        SNP_group = rep(c("Consensus", "Lead"), each = 3),
        fc = c(2.1, 1.5, 0.8, 1.2, 0.9, 1.1),
        FDR = c(0.01, 0.03, 0.1, 0.02, 0.05, 0.2),
        pvalue = c(0.001, 0.01, 0.05, 0.005, 0.02, 0.1),
        nOverlap = c(10, 8, 5, 7, 6, 4),
        Cell_type = rep(c("neurons", "microglia", "oligo"), 2),
        Assay = rep("ATAC", 6)
    )
    result <- suppressWarnings(
        echoannot:::XGR_enrichment_plot(
            enrich_res = enrich_res,
            plot_type = "point",
            show_plot = FALSE,
            save_plot = FALSE
        )
    )
    expect_true(methods::is(result, "ggplot"))
})

test_that("XGR_enrichment_plot with FDR_thresh filters results", {
    enrich_res <- data.table::data.table(
        SNP_group = c("Consensus", "Lead"),
        fc = c(2.1, 0.5),
        FDR = c(0.01, 0.5),
        pvalue = c(0.001, 0.3),
        nOverlap = c(10, 5),
        Cell_type = c("neurons", "microglia"),
        Assay = c("ATAC", "ATAC")
    )
    result <- echoannot:::XGR_enrichment_plot(
        enrich_res = enrich_res,
        plot_type = "bar",
        FDR_thresh = 0.05,
        show_plot = FALSE,
        save_plot = FALSE
    )
    expect_true(methods::is(result, "ggplot"))
})

test_that("XGR_plot_enrichment returns ggplot", {
    enrich_res <- data.table::data.table(
        source = c("SourceA", "SourceB"),
        assay = c("H3K27ac", "ATAC"),
        fc = c(2.5, 1.8),
        adjp = c(0.01, 0.03),
        Cell_type = c("neurons", "microglia"),
        Assay = c("H3K27ac", "ATAC")
    )
    result <- echoannot:::XGR_plot_enrichment(
        enrich_res = enrich_res,
        adjp_thresh = 0.05,
        show_plot = FALSE
    )
    expect_true(methods::is(result, "ggplot"))
})
