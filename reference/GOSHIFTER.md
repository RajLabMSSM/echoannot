# Run GoShifter enrichment pipeline

Run the [GoShifter](https://github.com/immunogenomics/goshifter)
locus-specific enrichment pipeline. This function orchestrates the full
workflow: preparing SNP maps and LD files, querying ROADMAP annotations,
running the GoShifter permutation test, and collecting results.

## Usage

``` r
GOSHIFTER(
  locus_dir,
  dat,
  SNP_group = "",
  goshifter_path = NULL,
  permutations = 1000,
  ROADMAP_search = "",
  chromatin_states = c("TssA"),
  R2_filter = 0.8,
  overlap_threshold = 1,
  force_new_goshifter = FALSE,
  remove_tmps = TRUE,
  verbose = TRUE,
  save_results = TRUE
)
```

## Source

[GoShifter GitHub](https://github.com/immunogenomics/goshifter) [Trynka
et al. (2015) *Am J Hum
Genet*](https://pubmed.ncbi.nlm.nih.gov/26140449/)

## Arguments

- locus_dir:

  Path to the locus-level results directory.

- dat:

  A `data.table` or `data.frame` with columns `SNP`, `CHR`, `POS`, and
  `P`.

- SNP_group:

  Character string labelling this group of SNPs (default `""`).

- goshifter_path:

  Path to the directory containing `goshifter.py`. If `NULL`, the
  bundled copy shipped with echolocatoR is used (see
  [`GOSHIFTER_find_folder`](https://rajlabmssm.github.io/echoannot/reference/GOSHIFTER_find_folder.md)).

- permutations:

  Number of permutations for the enrichment test (default `1000`).

- ROADMAP_search:

  A keyword query passed to
  [`ROADMAP_query`](https://rajlabmssm.github.io/echoannot/reference/ROADMAP_query.md)
  to filter ROADMAP annotations (e.g. `"monocyte"`).

- chromatin_states:

  Character vector of chromatin state abbreviations to test enrichment
  for (default `c("TssA")`).

- R2_filter:

  LD r-squared threshold for GoShifter (default `0.8`).

- overlap_threshold:

  Minimum number of overlapping SNPs required before running GoShifter
  on an annotation (default `1`).

- force_new_goshifter:

  If `TRUE`, re-run even if results already exist on disk (default
  `FALSE`).

- remove_tmps:

  Remove intermediate GoShifter output files after collecting results
  (default `TRUE`).

- verbose:

  Print messages (default `TRUE`).

- save_results:

  Write combined results to a tab-delimited file (default `TRUE`).

## Value

A `data.table` of GoShifter enrichment results across all requested
chromatin states.

## See also

Other GOSHIFTER:
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
[`GOSHIFTER_search_ROADMAP()`](https://rajlabmssm.github.io/echoannot/reference/GOSHIFTER_search_ROADMAP.md),
[`GOSHIFTER_summarise_results()`](https://rajlabmssm.github.io/echoannot/reference/GOSHIFTER_summarise_results.md)

## Examples

``` r
if (FALSE) { # \dontrun{
locus_dir <- echodata::locus_dir
dat <- echodata::BST1
gs_out <- GOSHIFTER(locus_dir = locus_dir, dat = dat)
} # }
```
