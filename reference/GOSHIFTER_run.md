# Run GoShifter enrichment test

Execute the [GoShifter](https://github.com/immunogenomics/goshifter)
Python tool on a set of SNPs against one or more annotation BED files.
Iterates over each element of `GRlist`, checks for overlap with the SNP
data, prepares annotation BED files, and calls the `goshifter.py`
script.

## Usage

``` r
GOSHIFTER_run(
  dat,
  locus_dir,
  GRlist,
  permutations = 1000,
  goshifter_path = NULL,
  chromatin_state = "TssA",
  R2_filter = 0.8,
  overlap_threshold = 1,
  remove_tmps = TRUE,
  verbose = TRUE
)
```

## Source

[GoShifter GitHub](https://github.com/immunogenomics/goshifter) [Trynka
et al. (2015) *Am J Hum
Genet*](https://pubmed.ncbi.nlm.nih.gov/26140449/)

## Arguments

- dat:

  A `data.table` or `data.frame` with at least columns `SNP`, `CHR`,
  `POS`, and `P`.

- locus_dir:

  Path to the locus-level results directory.

- GRlist:

  A named `GRangesList` of annotation regions to test for enrichment
  (e.g. from
  [`ROADMAP_query`](https://rajlabmssm.github.io/echoannot/reference/ROADMAP_query.md)).

- permutations:

  Number of permutations (default `1000`).

- goshifter_path:

  Path to the directory containing `goshifter.py`. If `NULL`, uses the
  bundled copy (see
  [`GOSHIFTER_find_folder`](https://rajlabmssm.github.io/echoannot/reference/GOSHIFTER_find_folder.md)).

- chromatin_state:

  Chromatin state label to record in the results (default `"TssA"`).

- R2_filter:

  LD r-squared threshold (default `0.8`).

- overlap_threshold:

  Minimum number of overlapping SNPs required to run GoShifter for a
  given annotation (default `1`).

- remove_tmps:

  Remove intermediate BED files after processing (default `TRUE`).

- verbose:

  Print messages (default `TRUE`).

## Value

A `data.table` with GoShifter results for all tested annotations.

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
[`GOSHIFTER_search_ROADMAP()`](https://rajlabmssm.github.io/echoannot/reference/GOSHIFTER_search_ROADMAP.md),
[`GOSHIFTER_summarise_results()`](https://rajlabmssm.github.io/echoannot/reference/GOSHIFTER_summarise_results.md)

## Examples

``` r
if (FALSE) { # \dontrun{
dat <- echodata::BST1
locus_dir <- echodata::locus_dir
peaks <- echoannot::NOTT2019_get_epigenomic_peaks()
grl_peaks <- GenomicRanges::makeGRangesListFromDataFrame(
    peaks, split.field = "Cell_type"
)
GS_results <- GOSHIFTER_run(
    dat = subset(dat, P < 5e-8),
    locus_dir = locus_dir,
    GRlist = grl_peaks
)
} # }
```
