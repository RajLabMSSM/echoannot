# Get widow limits

Get genomic window size limits for a locus plot.

## Usage

``` r
get_window_limits(
  dat,
  index_as_center = TRUE,
  zoom = NULL,
  genomic_units = "Mb",
  verbose = TRUE
)
```

## Arguments

- dat:

  Data.

- index_as_center:

  Use the index/lead SNP (the SNP with the smallest P-value) as the
  center point for the window.

- zoom:

  Zoom into the center of the locus when plotting (without editing the
  fine-mapping results file). You can provide either:

  - The size of your plot window in terms of basepairs (e.g.
    `zoom=50000` for a 50kb window).

  - How much you want to zoom in (e.g. `zoom="1x"` for the full locus,
    `zoom="2x"` for 2x zoom into the center of the locus, etc.).

  You can pass a list of window sizes (e.g. `c(50000,100000,500000)`) to
  automatically generate multiple views of each locus. This can even be
  a mix of different style inputs: e.g. `c("1x","4.5x",25000)`.

- genomic_units:

  Which genomic units to return window limits in.

- verbose:

  Print messages.

## See also

Other plot:
[`name_filter_convert()`](https://rajlabmssm.github.io/echoannot/reference/name_filter_convert.md)

## Examples

``` r
if (FALSE) { # \dontrun{
dat <- echodata::BST1
xlims <- get_window_limits(dat = dat, zoom = 50000)
xlims <- get_window_limits(dat = dat, zoom = "all")
xlims <- get_window_limits(dat = dat, zoom = "5x")
} # }
```
