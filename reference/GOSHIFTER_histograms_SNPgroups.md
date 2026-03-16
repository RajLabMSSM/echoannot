# GoShifter SNP-group histograms

Plot histograms of GoShifter p-values faceted by SNP group.

## Usage

``` r
GOSHIFTER_histograms_SNPgroups(GS_groups, show_plot = TRUE)
```

## Arguments

- GS_groups:

  A `data.table` of GoShifter results containing columns `SNP_group`,
  `Annotation`, `pval`, and `GS_pval`.

- show_plot:

  If `TRUE`, print the plot (default `TRUE`).

## Value

A `ggplot` object.

## See also

Other GOSHIFTER:
[`GOSHIFTER()`](https://rajlabmssm.github.io/echoannot/reference/GOSHIFTER.md),
[`GOSHIFTER_bed_names()`](https://rajlabmssm.github.io/echoannot/reference/GOSHIFTER_bed_names.md),
[`GOSHIFTER_check_overlap()`](https://rajlabmssm.github.io/echoannot/reference/GOSHIFTER_check_overlap.md),
[`GOSHIFTER_create_LD()`](https://rajlabmssm.github.io/echoannot/reference/GOSHIFTER_create_LD.md),
[`GOSHIFTER_create_snpmap()`](https://rajlabmssm.github.io/echoannot/reference/GOSHIFTER_create_snpmap.md),
[`GOSHIFTER_find_folder()`](https://rajlabmssm.github.io/echoannot/reference/GOSHIFTER_find_folder.md),
[`GOSHIFTER_get_roadmap_annotations()`](https://rajlabmssm.github.io/echoannot/reference/GOSHIFTER_get_roadmap_annotations.md),
[`GOSHIFTER_heatmap()`](https://rajlabmssm.github.io/echoannot/reference/GOSHIFTER_heatmap.md),
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
    SNP_group = rep(c("A", "B"), each = 5),
    Annotation = "E001",
    pval = runif(10),
    GS_pval = runif(10)
)
GOSHIFTER_histograms_SNPgroups(GS_groups)
} # }
```
