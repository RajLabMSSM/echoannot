# Plot XGR enrichment

Plot XGR enrichment.

## Usage

``` r
XGR_plot_enrichment(
  enrich_res,
  adjp_thresh = 0.05,
  top_annotations = NULL,
  show_plot = TRUE
)
```

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
[`XGR_prepare_foreground_background()`](https://rajlabmssm.github.io/echoannot/reference/XGR_prepare_foreground_background.md),
[`XGR_query()`](https://rajlabmssm.github.io/echoannot/reference/xgr_query.md),
[`XGR_sep_handler()`](https://rajlabmssm.github.io/echoannot/reference/XGR_sep_handler.md),
[`xgr_example`](https://rajlabmssm.github.io/echoannot/reference/xgr_example.md)

## Examples

``` r
if (FALSE) { # \dontrun{
enrich_res <- XGR_iterate_enrichment(
    dat = echodata::get_Nalls2019_merged(),
    foreground_filter = "Consensus_SNP",
    background_filter = "leadSNP",
    lib.selections = c("ENCODE_TFBS_ClusteredV3_CellTypes")
)
XGR_plot_enrichment(enrich_res)
} # }
```
