# Search ROADMAP annotation reference for GoShifter

Search the ROADMAP Epigenomic reference file for annotation BED files
matching filter criteria or a fuzzy keyword search.

## Usage

``` r
GOSHIFTER_search_ROADMAP(
  Roadmap_reference = system.file("extdata/ROADMAP", "ROADMAP_Epigenomic.js", package =
    "echoannot"),
  EID_filter = NA,
  GROUP_filter = NA,
  ANATOMY_filter = NA,
  GR_filter = NA,
  fuzzy_search = NA,
  verbose = TRUE
)
```

## Arguments

- Roadmap_reference:

  Path to the ROADMAP reference file (default: bundled file in
  echoannot).

- EID_filter:

  Filter by Epigenome ID(s).

- GROUP_filter:

  Filter by GROUP column.

- ANATOMY_filter:

  Filter by ANATOMY column.

- GR_filter:

  Filter by GR column.

- fuzzy_search:

  Keyword(s) to search across all annotation columns (case-insensitive).

- verbose:

  Print messages (default `TRUE`).

## Value

A `data.table` of matching ROADMAP annotation entries.

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
[`GOSHIFTER_summarise_results()`](https://rajlabmssm.github.io/echoannot/reference/GOSHIFTER_summarise_results.md)
