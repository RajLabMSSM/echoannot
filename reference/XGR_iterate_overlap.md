# Check overlap with XGR annotations

Automatically handles different file formats provided by XGR (e.g.
varying kinds of nested/unnested `GRanges`). Then returns a `Granges`
object with only the XGR annotation ranges that overlap with the SNPs in
`dat`. The `GRanges` merges hits from `dat`.

## Usage

``` r
XGR_iterate_overlap(
  lib.selections = c("ENCODE_TFBS_ClusteredV3_CellTypes", "TFBS_Conserved",
    "ReMap_PublicAndEncode_TFBS", "Uniform_TFBS"),
  dat,
  save_path = FALSE,
  nThread = 1
)
```

## Arguments

- lib.selections:

  Which XGR annotations to check overlap with. For full list of
  libraries see [here (XGR on
  CRAN).](https://cran.r-project.org/package=XGR) Passed to the
  `RData.customised` argument in `XGR::xRDataLoader`.

- dat:

  Data.frame with at least the following columns:

  SNP

  :   SNP RSID

  CHR

  :   chromosome

  POS

  :   position

- save_path:

  Save the results as a `data.frame`.

- nThread:

  Number of threads to parallelise across libraries.

## See also

Other XGR:
[`XGR_enrichment()`](https://rajlabmssm.github.io/echoannot/reference/XGR_enrichment.md),
[`XGR_enrichment_bootstrap()`](https://rajlabmssm.github.io/echoannot/reference/XGR_enrichment_bootstrap.md),
[`XGR_enrichment_plot()`](https://rajlabmssm.github.io/echoannot/reference/XGR_enrichment_plot.md),
[`XGR_filter_assays()`](https://rajlabmssm.github.io/echoannot/reference/XGR_filter_assays.md),
[`XGR_filter_sources()`](https://rajlabmssm.github.io/echoannot/reference/XGR_filter_sources.md),
[`XGR_import_annotations()`](https://rajlabmssm.github.io/echoannot/reference/XGR_import_annotations.md),
[`XGR_iterate_enrichment()`](https://rajlabmssm.github.io/echoannot/reference/XGR_iterate_enrichment.md),
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
gr.hits <- XGR_iterate_overlap(
    lib.selections = c("ENCODE_TFBS_ClusteredV3_CellTypes"),
    dat = echodata::BST1
)
} # }
```
