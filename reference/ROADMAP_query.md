# Query Roadmap Query Roadmap annotations using a set of genomic coordinates.

Query Roadmap

Query Roadmap annotations using a set of genomic coordinates.

## Usage

``` r
ROADMAP_query(
  query_dat,
  keyword_query = NULL,
  limit_files = NULL,
  chrom_states = NULL,
  n_top = NULL,
  min_hits = 1,
  save_dir = file.path(tempdir(), paste(paste0("roadmap_query--", paste(keyword_query,
    collapse = "-")), paste0("n_top--", n_top), paste0("limit_files--", limit_files),
    paste(chrom_states, collapse = "-"), sep = ".")),
  force_new = FALSE,
  return_paths = FALSE,
  merge_and_process = FALSE,
  conda_env = "echoR_mini",
  nThread = 1,
  verbose = TRUE
)
```

## Arguments

- query_dat:

  Variant-level summary statistics.

- keyword_query:

  Search all columns in the Roadmap annotations metadata and only query
  annotations that contain your keywords. Can provide multiple keywords
  in list form: `c("placenta","liver","monocytes")`.

- limit_files:

  Limit the number of annotation files queried (for faster testing).

- chrom_states:

  Filter results by chromatin states.

- n_top:

  The number of top annotation sources (e.g. tissues) to include, sorted
  by greatest number of rows (i.e. the number of genomic ranges within
  the window).

- min_hits:

  Minimum number of hits (regions overlapping with `query_dat`) required
  to include a given annotation.

- save_dir:

  Directory to store query results in.

- force_new:

  Overwrite any existing files of the same name.

- return_paths:

  Return list of paths instead of a
  [GRangesList](https://rdrr.io/pkg/GenomicRanges/man/GRangesList-class.html).

- merge_and_process:

  Perform filtering and merging of
  [GRangesList](https://rdrr.io/pkg/GenomicRanges/man/GRangesList-class.html)
  items.

- conda_env:

  Conda environment to search for tabix in.

- nThread:

  Number of threads to parallelise queries over.

- verbose:

  Print messages.

## See also

Other ROADMAP:
[`ROADMAP_construct_reference()`](https://rajlabmssm.github.io/echoannot/reference/ROADMAP_construct_reference.md),
[`ROADMAP_merge_and_process()`](https://rajlabmssm.github.io/echoannot/reference/ROADMAP_merge_and_process.md),
[`ROADMAP_tabix()`](https://rajlabmssm.github.io/echoannot/reference/ROADMAP_tabix.md)

## Examples

``` r
if (FALSE) { # \dontrun{
query_dat <- echodata::BST1
grl <- ROADMAP_query(
    query_dat = query_dat,
    keyword_query = "monocyte")
} # }
```
