# Conduct enrichment tests for each annotation

XGR uses a binomial enrichment tests for each annotation.

## Usage

``` r
XGR_iterate_enrichment(
  dat,
  foreground_filter = "Consensus_SNP",
  background_filter = "leadSNP",
  lib.selections = c("ENCODE_TFBS_ClusteredV3_CellTypes",
    "ENCODE_DNaseI_ClusteredV3_CellTypes", "Broad_Histone", "FANTOM5_Enhancer",
    "Segment_Combined_Gm12878", "TFBS_Conserved", "ReMap_PublicAndEncode_TFBS",
    "Blueprint_VenousBlood_Histone", "Blueprint_DNaseI", "FANTOM5_CAT_Cell",
    "FANTOM5_CAT_MESH", "GWAScatalog_alltraits"),
  save_path = FALSE,
  nThread = 1
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

## Details

[Description of all
datasets](https://www.rdocumentation.org/packages/XGR/versions/1.1.5/topics/xDefineGenomicAnno)

## See also

Other XGR:
[`XGR_enrichment()`](https://rajlabmssm.github.io/echoannot/reference/XGR_enrichment.md),
[`XGR_enrichment_bootstrap()`](https://rajlabmssm.github.io/echoannot/reference/XGR_enrichment_bootstrap.md),
[`XGR_enrichment_plot()`](https://rajlabmssm.github.io/echoannot/reference/XGR_enrichment_plot.md),
[`XGR_filter_assays()`](https://rajlabmssm.github.io/echoannot/reference/XGR_filter_assays.md),
[`XGR_filter_sources()`](https://rajlabmssm.github.io/echoannot/reference/XGR_filter_sources.md),
[`XGR_import_annotations()`](https://rajlabmssm.github.io/echoannot/reference/XGR_import_annotations.md),
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
enrich_res <- XGR_iterate_enrichment(
    dat = echodata::get_Nalls2019_merged(),
    foreground_filter = "Consensus_SNP",
    background_filter = "leadSNP",
    lib.selections = c("ENCODE_TFBS_ClusteredV3_CellTypes")
)
} # }
```
