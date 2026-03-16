# Import filtered bigwig

Import a subset of a bigwig file based on the coordinates in a
[GRanges](https://rdrr.io/pkg/GenomicRanges/man/GRanges-class.html)
object (`gr.query_dat`).

## Usage

``` r
import_bigwig_filtered(bw.file, gr.query_dat, full_data = TRUE)
```

## Arguments

- bw.file:

  Path to a bigwig file.

- gr.query_dat:

  [GRanges](https://rdrr.io/pkg/GenomicRanges/man/GRanges-class.html)
  object to query the bigwig file with.

- full_data:

  Whether to return the actual read ranges (`full_data=TRUE`), or just
  the "score" column which summarizes the height of the aggregated reads
  across the genome (`full_data=TRUE`).

## Value

[GRanges](https://rdrr.io/pkg/GenomicRanges/man/GRanges-class.html)
