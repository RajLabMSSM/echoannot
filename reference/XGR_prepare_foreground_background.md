# Prepare SNP sets for enrichment

Prepare custom foreground and background SNPs sets for enrichment tests
with XGR annotations.

## Usage

``` r
XGR_prepare_foreground_background(
  dat,
  foreground_filter = "Support>0",
  background_filter = NULL,
  fg_sample_size = NULL,
  bg_sample_size = NULL,
  verbose = TRUE
)
```

## Arguments

- dat:

  Data.frame with at least the following columns:

  SNP

  :   SNP RSID

  CHR

  :   chromosome

  POS

  :   position

- foreground_filter:

  Specify foreground by filtering SNPs in `dat`. Write filter as a
  string (or `NULL` to include all SNPs).

- background_filter:

  Specify background by filtering SNPs in `dat`. Write filter as a
  string (or `NULL` to include all SNPs).

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
[`XGR_query()`](https://rajlabmssm.github.io/echoannot/reference/xgr_query.md),
[`XGR_sep_handler()`](https://rajlabmssm.github.io/echoannot/reference/XGR_sep_handler.md),
[`xgr_example`](https://rajlabmssm.github.io/echoannot/reference/xgr_example.md)

## Examples

``` r
if (FALSE) { # \dontrun{
fg_bg <- echoannot:::XGR_prepare_foreground_background(
    dat = echodata::get_Nalls2019_merged(),
    foreground_filter = "Consensus_SNP==TRUE",
    background_filter = "leadSNP==TRUE"
)
} # }
```
