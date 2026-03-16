# Filter chromatin states

Annotate and filter a
[GRangesList](https://rdrr.io/pkg/GenomicRanges/man/GRangesList-class.html)
using descriptions of chromatin states from ROADMAP metadata.

## Usage

``` r
filter_chromatin_states(
  grl,
  chrom_states = NULL,
  chrom_states_col = "name",
  verbose = TRUE
)
```

## Arguments

- grl:

  A
  [GRangesList](https://rdrr.io/pkg/GenomicRanges/man/GRangesList-class.html).

- chrom_states:

  Chromatin states to keep. If `NULL` (default), data will not be
  filtered at all (only annotated).

- chrom_states_col:

  Column name in which chromatin state info is stored.

- verbose:

  Print messages.

## Value

Annotated/filtered
[GRangesList](https://rdrr.io/pkg/GenomicRanges/man/GRangesList-class.html).
