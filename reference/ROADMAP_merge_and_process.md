# Standardize Roadmap query

Standardize and filter Roadmap query results.

## Usage

``` r
ROADMAP_merge_and_process(
  grl.roadmap,
  gr.snp,
  n_top = NULL,
  minoverlap = 1,
  verbose = TRUE
)
```

## Arguments

- grl.roadmap:

  Roadmap query results.

- n_top:

  The number of top annotation sources (e.g. tissues) to include, sorted
  by greatest number of rows (i.e. the number of genomic ranges within
  the window).

- minoverlap:

  A single non-negative integer.

  Only ranges with a minimum of `minoverlap` overlapping positions are
  considered to be overlapping.

  When `type` is `"any"`, at least one of `maxgap` and `minoverlap` must
  be set to its default value.

- verbose:

  Print messages.

## See also

Other ROADMAP:
[`ROADMAP_construct_reference()`](https://rajlabmssm.github.io/echoannot/reference/ROADMAP_construct_reference.md),
[`ROADMAP_query()`](https://rajlabmssm.github.io/echoannot/reference/ROADMAP_query.md),
[`ROADMAP_tabix()`](https://rajlabmssm.github.io/echoannot/reference/ROADMAP_tabix.md)
