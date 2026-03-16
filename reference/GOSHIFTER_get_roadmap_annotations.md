# Download and subset ROADMAP annotations for GoShifter

Download ROADMAP chromHMM BED files from the WashU Epigenome Browser,
optionally subset to a specific chromatin state, and return paths to the
processed files.

## Usage

``` r
GOSHIFTER_get_roadmap_annotations(
  annotations_path = "./annotations",
  bed_list,
  chromatin_state = "TssA",
  verbose = TRUE
)
```

## Arguments

- annotations_path:

  Directory where annotation files are stored/cached.

- bed_list:

  Character vector of BED file names (as produced by
  [`GOSHIFTER_bed_names`](https://rajlabmssm.github.io/echoannot/reference/GOSHIFTER_bed_names.md)).

- chromatin_state:

  Chromatin state to subset by (e.g. `"TssA"`). Set to `NA` to keep all
  states.

- verbose:

  Print messages (default `TRUE`).

## Value

A character vector of paths to processed BED files (gzipped).

## See also

Other GOSHIFTER:
[`GOSHIFTER()`](https://rajlabmssm.github.io/echoannot/reference/GOSHIFTER.md),
[`GOSHIFTER_bed_names()`](https://rajlabmssm.github.io/echoannot/reference/GOSHIFTER_bed_names.md),
[`GOSHIFTER_check_overlap()`](https://rajlabmssm.github.io/echoannot/reference/GOSHIFTER_check_overlap.md),
[`GOSHIFTER_create_LD()`](https://rajlabmssm.github.io/echoannot/reference/GOSHIFTER_create_LD.md),
[`GOSHIFTER_create_snpmap()`](https://rajlabmssm.github.io/echoannot/reference/GOSHIFTER_create_snpmap.md),
[`GOSHIFTER_find_folder()`](https://rajlabmssm.github.io/echoannot/reference/GOSHIFTER_find_folder.md),
[`GOSHIFTER_heatmap()`](https://rajlabmssm.github.io/echoannot/reference/GOSHIFTER_heatmap.md),
[`GOSHIFTER_histograms_SNPgroups()`](https://rajlabmssm.github.io/echoannot/reference/GOSHIFTER_histograms_SNPgroups.md),
[`GOSHIFTER_histograms_pvals()`](https://rajlabmssm.github.io/echoannot/reference/GOSHIFTER_histograms_pvals.md),
[`GOSHIFTER_list_chromatin_states()`](https://rajlabmssm.github.io/echoannot/reference/GOSHIFTER_list_chromatin_states.md),
[`GOSHIFTER_process_results()`](https://rajlabmssm.github.io/echoannot/reference/GOSHIFTER_process_results.md),
[`GOSHIFTER_run()`](https://rajlabmssm.github.io/echoannot/reference/GOSHIFTER_run.md),
[`GOSHIFTER_search_ROADMAP()`](https://rajlabmssm.github.io/echoannot/reference/GOSHIFTER_search_ROADMAP.md),
[`GOSHIFTER_summarise_results()`](https://rajlabmssm.github.io/echoannot/reference/GOSHIFTER_summarise_results.md)
