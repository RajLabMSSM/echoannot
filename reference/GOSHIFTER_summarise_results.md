# Summarise GoShifter results

Filter GoShifter results to significant enrichments and report
per-tissue counts.

## Usage

``` r
GOSHIFTER_summarise_results(
  GS_results,
  roadmap_ref = NULL,
  chromatin_state = NULL,
  verbose = TRUE
)
```

## Arguments

- GS_results:

  A `data.table` of GoShifter results (as returned by
  [`GOSHIFTER`](https://rajlabmssm.github.io/echoannot/reference/GOSHIFTER.md)
  or
  [`GOSHIFTER_run`](https://rajlabmssm.github.io/echoannot/reference/GOSHIFTER_run.md)).

- roadmap_ref:

  A ROADMAP reference `data.table` (from
  [`ROADMAP_construct_reference`](https://rajlabmssm.github.io/echoannot/reference/ROADMAP_construct_reference.md)).
  If `NULL`, it is constructed automatically.

- chromatin_state:

  Character vector of chromatin states tested.

- verbose:

  Print messages (default `TRUE`).

## Value

A `data.table` of significant results (enrichment p-value \<= 0.05),
arranged by enrichment.

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
[`GOSHIFTER_histograms_SNPgroups()`](https://rajlabmssm.github.io/echoannot/reference/GOSHIFTER_histograms_SNPgroups.md),
[`GOSHIFTER_histograms_pvals()`](https://rajlabmssm.github.io/echoannot/reference/GOSHIFTER_histograms_pvals.md),
[`GOSHIFTER_list_chromatin_states()`](https://rajlabmssm.github.io/echoannot/reference/GOSHIFTER_list_chromatin_states.md),
[`GOSHIFTER_process_results()`](https://rajlabmssm.github.io/echoannot/reference/GOSHIFTER_process_results.md),
[`GOSHIFTER_run()`](https://rajlabmssm.github.io/echoannot/reference/GOSHIFTER_run.md),
[`GOSHIFTER_search_ROADMAP()`](https://rajlabmssm.github.io/echoannot/reference/GOSHIFTER_search_ROADMAP.md)
