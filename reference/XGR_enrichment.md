# XGR enrichment

Run SNP-level enrichment test with `XGR::xGRviaGenomicAnno`.

## Usage

``` r
XGR_enrichment(
  gr,
  merged_dat,
  foreground_filter = "Consensus_SNP==TRUE",
  background_filter = NULL,
  grouping_vars = c("Study", "Assay", "Cell_type"),
  fg_sample_size = NULL,
  bg_sample_size = NULL,
  background.annotatable.only = FALSE,
  verbose = TRUE
)
```

## Arguments

- gr:

  Annotations to test for enrichment with.

- merged_dat:

  SNP-level fine-mapping results to test for enrichment with.

- foreground_filter:

  Filter to apply to foreground (target SNPs).

- background_filter:

  Filter to apply to background (non-target SNPs).

- grouping_vars:

  Columns in `merged_dat` to group by when conducting enrichment tests.

- fg_sample_size:

  Foreground sample size.

- bg_sample_size:

  Background sample size.

- background.annotatable.only:

  For background SNPs, only use SNPs that overlap with some annotation
  in `gr`. This means that missing annotations (`NA`) will not be
  considered.

- verbose:

  Print messages.

## See also

Other XGR:
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
[`XGR_sep_handler()`](https://rajlabmssm.github.io/echoannot/reference/XGR_sep_handler.md),
[`xgr_example`](https://rajlabmssm.github.io/echoannot/reference/xgr_example.md)

## Examples

``` r
if (FALSE) { # \dontrun{
gr.merged <- echoannot::merge_celltype_specific_epigenomics()
enrich.lead <- XGR_enrichment(
    gr = gr.merged,
    merged_dat = echodata::get_Nalls2019_merged(),
    foreground_filter = "leadSNP==TRUE",
    grouping_vars = c("Study", "Cell_type", "Assay")
)
} # }
```
