# Filter assays

Identify the assays with the most annotations in the locus. Then only
keep these assays.

## Usage

``` r
XGR_filter_assays(gr.lib, n_top_assays = 1)
```

## Arguments

- gr.lib:

  Results from
  [XGR_query](https://rajlabmssm.github.io/echoannot/reference/xgr_query.md).

- n_top_assays:

  Number of top assays to include per library.

## See also

Other XGR:
[`XGR_enrichment()`](https://rajlabmssm.github.io/echoannot/reference/XGR_enrichment.md),
[`XGR_enrichment_bootstrap()`](https://rajlabmssm.github.io/echoannot/reference/XGR_enrichment_bootstrap.md),
[`XGR_enrichment_plot()`](https://rajlabmssm.github.io/echoannot/reference/XGR_enrichment_plot.md),
[`XGR_filter_sources()`](https://rajlabmssm.github.io/echoannot/reference/XGR_filter_sources.md),
[`XGR_import_annotations()`](https://rajlabmssm.github.io/echoannot/reference/XGR_import_annotations.md),
[`XGR_iterate_enrichment()`](https://rajlabmssm.github.io/echoannot/reference/XGR_iterate_enrichment.md),
[`XGR_iterate_overlap()`](https://rajlabmssm.github.io/echoannot/reference/XGR_iterate_overlap.md),
[`XGR_merge_and_process()`](https://rajlabmssm.github.io/echoannot/reference/XGR_merge_and_process.md),
[`XGR_parse_metadata()`](https://rajlabmssm.github.io/echoannot/reference/XGR_parse_metadata.md),
[`XGR_plot_enrichment()`](https://rajlabmssm.github.io/echoannot/reference/XGR_plot_enrichment.md),
[`XGR_prepare_foreground_background()`](https://rajlabmssm.github.io/echoannot/reference/XGR_prepare_foreground_background.md),
[`XGR_query()`](https://rajlabmssm.github.io/echoannot/reference/xgr_query.md),
[`XGR_sep_handler()`](https://rajlabmssm.github.io/echoannot/reference/XGR_sep_handler.md),
[`xgr_example`](https://rajlabmssm.github.io/echoannot/reference/xgr_example.md)

## Examples

``` r
if (FALSE) { # \dontrun{
gr.filt <- echoannot::XGR_filter_assays(gr.lib = echoannot::xgr_example)
} # }
```
