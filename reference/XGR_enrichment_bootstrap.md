# XGR enrichment (bootstrapped)

Perform annotation enrichment tests using iterative bootstrapping
procedure.

## Usage

``` r
XGR_enrichment_bootstrap(
  gr,
  merged_dat,
  snp_groups = c("Random", "GWAS lead", "UCS (-PolyFun)", "UCS", "Consensus (-PolyFun)",
    "Consensus"),
  background_filter = NULL,
  grouping_vars = c("Study", "Assay", "Cell_type"),
  iterations = 1000,
  fg_sample_size = 20,
  bg_sample_size = NULL,
  bootstrap = TRUE,
  save_path = tempfile(fileext = "XGR_enrich_boot_res.csv.gz"),
  nThread = 1,
  verbose = TRUE
)
```

## Arguments

- gr:

  Annotations to test for enrichment with.

- merged_dat:

  SNP-level fine-mapping results to test for enrichment with.

- snp_groups:

  Which SNP groups to repeat enrichment tests for separately.

- background_filter:

  Filter to apply to background (non-target SNPs).

- grouping_vars:

  Columns in `merged_dat` to group by when conducting enrichment tests.

- iterations:

  Number of bootstrapping iterations.

- fg_sample_size:

  Foreground sample size.

- bg_sample_size:

  Background sample size.

- bootstrap:

  Whether to use bootstrapping.

- save_path:

  File path to save results to.

- nThread:

  Number of threads to parallelise bootstrapping over.

- verbose:

  Print messages.

## See also

Other XGR:
[`XGR_enrichment()`](https://rajlabmssm.github.io/echoannot/reference/XGR_enrichment.md),
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
enrich_res <- XGR_enrichment_bootstrap(
    gr = gr.merged,
    merged_dat = echodata::get_Nalls2019_merged()
)
} # }
```
