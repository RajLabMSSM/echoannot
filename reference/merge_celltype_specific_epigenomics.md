# Merge all cell-type-specific epigenomics

Merges multiple cell-type-specific epigenomic datasets (Nott 2019,
Corces 2020) into a single
[GRanges](https://rdrr.io/pkg/GenomicRanges/man/GRanges-class.html)
object.

## Usage

``` r
merge_celltype_specific_epigenomics(
  keep_extra_cols = FALSE,
  save_path = file.path(tools::R_user_dir(package = "echoannot", which = "cache"),
    "merge_celltype_specific_epigenomics.rds"),
  force_new = FALSE,
  verbose = TRUE
)
```

## Arguments

- keep_extra_cols:

  Keep extra columns that are not shared across all annotations.

- save_path:

  Path to save merged results to.

- force_new:

  If cached merged results already exist, ignore them and recreate the
  file anyway.

- verbose:

  Print messages.

## Examples

``` r
if (FALSE) { # \dontrun{
gr.merged <- echoannot::merge_celltype_specific_epigenomics()
} # }
```
