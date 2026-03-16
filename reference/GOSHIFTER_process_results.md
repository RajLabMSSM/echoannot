# Process GoShifter output files

Read the `.locusscore` and `.enrich` output files written by GoShifter
and merge them with ROADMAP reference metadata.

## Usage

``` r
GOSHIFTER_process_results(locus_dir, output_bed, out_prefix)
```

## Arguments

- locus_dir:

  Path to the locus-level results directory.

- output_bed:

  Path to the annotation BED file that was tested.

- out_prefix:

  Prefix used when writing GoShifter output files (typically the
  annotation name).

## Value

A `data.table` with per-locus scores, p-values, and ROADMAP metadata.

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
[`GOSHIFTER_run()`](https://rajlabmssm.github.io/echoannot/reference/GOSHIFTER_run.md),
[`GOSHIFTER_search_ROADMAP()`](https://rajlabmssm.github.io/echoannot/reference/GOSHIFTER_search_ROADMAP.md),
[`GOSHIFTER_summarise_results()`](https://rajlabmssm.github.io/echoannot/reference/GOSHIFTER_summarise_results.md)
