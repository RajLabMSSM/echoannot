# Example XGR query

Example XGR query results from XGR via
[XGR_query](https://rajlabmssm.github.io/echoannot/reference/xgr_query.md)
using the "ENCODE_DNaseI_ClusteredV3_CellTypes" dataset.

## Usage

``` r
data("xgr_example")
```

## Format

An object of class `GRanges` of length 8490.

## Source

` xgr_example <- echoannot::XGR_query( c("ENCODE_DNaseI_ClusteredV3_CellTypes"), dat = echodata::BST1) `
usethis::use_data(xgr_example, overwrite = TRUE)

## See also

Other XGR:
[`XGR_enrichment()`](https://rajlabmssm.github.io/echoannot/reference/XGR_enrichment.md),
[`XGR_enrichment_bootstrap()`](https://rajlabmssm.github.io/echoannot/reference/XGR_enrichment_bootstrap.md),
[`XGR_enrichment_plot()`](https://rajlabmssm.github.io/echoannot/reference/XGR_enrichment_plot.md),
[`XGR_filter_assays()`](https://rajlabmssm.github.io/echoannot/reference/XGR_filter_assays.md),
[`XGR_filter_sources()`](https://rajlabmssm.github.io/echoannot/reference/XGR_filter_sources.md),
[`XGR_import_annotations()`](https://rajlabmssm.github.io/echoannot/reference/XGR_import_annotations.md),
[`XGR_iterate_enrichment()`](https://rajlabmssm.github.io/echoannot/reference/XGR_iterate_enrichment.md),
[`XGR_iterate_overlap()`](https://rajlabmssm.github.io/echoannot/reference/XGR_iterate_overlap.md),
[`XGR_merge_and_process()`](https://rajlabmssm.github.io/echoannot/reference/XGR_merge_and_process.md),
[`XGR_parse_metadata()`](https://rajlabmssm.github.io/echoannot/reference/XGR_parse_metadata.md),
[`XGR_plot_enrichment()`](https://rajlabmssm.github.io/echoannot/reference/XGR_plot_enrichment.md),
[`XGR_prepare_foreground_background()`](https://rajlabmssm.github.io/echoannot/reference/XGR_prepare_foreground_background.md),
[`XGR_query()`](https://rajlabmssm.github.io/echoannot/reference/xgr_query.md),
[`XGR_sep_handler()`](https://rajlabmssm.github.io/echoannot/reference/XGR_sep_handler.md)
