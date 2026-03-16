# Create GoShifter LD files

Prepare LD input files for GoShifter. Accepts either a sparse LD matrix
RDS file (from echoLD) or a PLINK `.ld` file. The output is a set of
per-chromosome, bgzipped and tabix-indexed LD files.

## Usage

``` r
GOSHIFTER_create_LD(
  locus_dir,
  dat = NULL,
  LD_path = NULL,
  conda_env = "goshifter",
  nThread = 1,
  verbose = TRUE
)
```

## Arguments

- locus_dir:

  Path to the locus-level results directory.

- dat:

  A `data.table` or `data.frame` with at least columns `SNP`, `CHR`, and
  `POS`. If `NULL`, will attempt to read from the multi-finemap results
  in `locus_dir`.

- LD_path:

  Path to the LD file. If `NULL`, the first `.RDS` file in
  `<locus_dir>/LD/` is used.

- conda_env:

  Conda environment name in which `bgzip` and `tabix` can be found
  (default `"goshifter"`).

- nThread:

  Number of threads for file I/O (default `1`).

- verbose:

  Print messages (default `TRUE`).

## Value

Path to the LD output folder (invisibly).

## See also

Other GOSHIFTER:
[`GOSHIFTER()`](https://rajlabmssm.github.io/echoannot/reference/GOSHIFTER.md),
[`GOSHIFTER_bed_names()`](https://rajlabmssm.github.io/echoannot/reference/GOSHIFTER_bed_names.md),
[`GOSHIFTER_check_overlap()`](https://rajlabmssm.github.io/echoannot/reference/GOSHIFTER_check_overlap.md),
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
dat <- echodata::BST1
locus_dir <- echodata::locus_dir
LD_folder <- GOSHIFTER_create_LD(
    locus_dir = locus_dir,
    dat = subset(dat, P < 5e-8)
)
} # }
```
