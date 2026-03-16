# Download, standardize, and merge XGR annotations

Merges a list of XGR annotations into a single
[GRangesList](https://rdrr.io/pkg/GenomicRanges/man/GRangesList-class.html)
(or merged
[GRanges](https://rdrr.io/pkg/GenomicRanges/man/GRanges-class.html))
object.

## Usage

``` r
XGR_query(
  lib.selections = c("ENCODE_TFBS_ClusteredV3_CellTypes", "TFBS_Conserved",
    "Uniform_TFBS"),
  as_grangesList = FALSE,
  dat = NULL,
  n_top = NULL,
  nThread = 1
)
```

## Arguments

- lib.selections:

  Which XGR annotations to check overlap with. For full list of
  libraries see [here (XGR on
  CRAN).](https://cran.r-project.org/package=XGR) Passed to the
  `RData.customised` argument in `XGR::xRDataLoader`.

- as_grangesList:

  Return as a
  [GRangesList](https://rdrr.io/pkg/GenomicRanges/man/GRangesList-class.html),
  instead of a single merged
  [GRanges](https://rdrr.io/pkg/GenomicRanges/man/GRanges-class.html)
  object.

- dat:

  data.table of genomic coordinates to query with. Set as `NULL` to
  return genome-wide data.

- n_top:

  Filter to only the top N annotations that have the greatest amount of
  overlap with the genomic coordinates of `dat`.

- nThread:

  Number of cores to parallelise across.

## Value

[GRangesList](https://rdrr.io/pkg/GenomicRanges/man/GRangesList-class.html)

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
[`XGR_sep_handler()`](https://rajlabmssm.github.io/echoannot/reference/XGR_sep_handler.md),
[`xgr_example`](https://rajlabmssm.github.io/echoannot/reference/xgr_example.md)

## Examples

``` r
if (FALSE) { # \dontrun{
gr.lib <- echoannot::XGR_query(
    lib.selections = c("ENCODE_DNaseI_ClusteredV3_CellTypes"),
    dat = echodata::BST1,
    n_top = 1)
} # }
```
