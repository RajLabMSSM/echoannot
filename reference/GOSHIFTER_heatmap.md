# GoShifter enrichment heatmap

Create an interactive heatmap of GoShifter enrichment results for
significant tests (p \<= 0.05), using heatmaply.

## Usage

``` r
GOSHIFTER_heatmap(GS_groups, show_plot = TRUE)
```

## Arguments

- GS_groups:

  A `data.table` of GoShifter results containing columns `pval`,
  `Epigenome.name`, `chromatin_state`, `SNP_group`, and
  `mean_enrichment`.

- show_plot:

  If `TRUE`, print the heatmap (default `TRUE`).

## Value

A heatmaply heatmap object.

## See also

Other GOSHIFTER:
[`GOSHIFTER()`](https://rajlabmssm.github.io/echoannot/reference/GOSHIFTER.md),
[`GOSHIFTER_bed_names()`](https://rajlabmssm.github.io/echoannot/reference/GOSHIFTER_bed_names.md),
[`GOSHIFTER_check_overlap()`](https://rajlabmssm.github.io/echoannot/reference/GOSHIFTER_check_overlap.md),
[`GOSHIFTER_create_LD()`](https://rajlabmssm.github.io/echoannot/reference/GOSHIFTER_create_LD.md),
[`GOSHIFTER_create_snpmap()`](https://rajlabmssm.github.io/echoannot/reference/GOSHIFTER_create_snpmap.md),
[`GOSHIFTER_find_folder()`](https://rajlabmssm.github.io/echoannot/reference/GOSHIFTER_find_folder.md),
[`GOSHIFTER_get_roadmap_annotations()`](https://rajlabmssm.github.io/echoannot/reference/GOSHIFTER_get_roadmap_annotations.md),
[`GOSHIFTER_histograms_SNPgroups()`](https://rajlabmssm.github.io/echoannot/reference/GOSHIFTER_histograms_SNPgroups.md),
[`GOSHIFTER_histograms_pvals()`](https://rajlabmssm.github.io/echoannot/reference/GOSHIFTER_histograms_pvals.md),
[`GOSHIFTER_list_chromatin_states()`](https://rajlabmssm.github.io/echoannot/reference/GOSHIFTER_list_chromatin_states.md),
[`GOSHIFTER_process_results()`](https://rajlabmssm.github.io/echoannot/reference/GOSHIFTER_process_results.md),
[`GOSHIFTER_run()`](https://rajlabmssm.github.io/echoannot/reference/GOSHIFTER_run.md),
[`GOSHIFTER_search_ROADMAP()`](https://rajlabmssm.github.io/echoannot/reference/GOSHIFTER_search_ROADMAP.md),
[`GOSHIFTER_summarise_results()`](https://rajlabmssm.github.io/echoannot/reference/GOSHIFTER_summarise_results.md)

## Examples

``` r
if (FALSE) { # \dontrun{
GS_groups <- data.table::data.table(
    pval = c(0.01, 0.03, 0.8),
    Epigenome.name = c("A", "B", "C"),
    chromatin_state = "TssA",
    SNP_group = "group1",
    mean_enrichment = c(0.5, 0.3, 0.1)
)
GOSHIFTER_heatmap(GS_groups)
} # }
```
