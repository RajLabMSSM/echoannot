# Find GenomicRanges overlap Find overlap genomic position overlap between two [GRanges](https://rdrr.io/pkg/GenomicRanges/man/GRanges-class.html) objects.

Find GenomicRanges overlap

Find overlap genomic position overlap between two
[GRanges](https://rdrr.io/pkg/GenomicRanges/man/GRanges-class.html)
objects.

## Usage

``` r
granges_overlap(
  dat1,
  dat2,
  chrom_col.1 = "chrom",
  start_col.1 = "start",
  end_col.1 = start_col.1,
  chrom_col.2 = "chrom",
  start_col.2 = "start",
  end_col.2 = end_col.2,
  return_merged = TRUE,
  unique_only = TRUE,
  style = "NCBI",
  verbose = FALSE
)
```

## Arguments

- dat1:

  Dataset 1 (can be
  [GRanges](https://rdrr.io/pkg/GenomicRanges/man/GRanges-class.html) or
  [data.table](https://rdrr.io/pkg/data.table/man/data.table.html)).

- dat2:

  Dataset 2. (can be
  [GRanges](https://rdrr.io/pkg/GenomicRanges/man/GRanges-class.html) or
  [data.table](https://rdrr.io/pkg/data.table/man/data.table.html)).

- chrom_col.1:

  Name of the chromosome column in `dat1`.

- start_col.1:

  Name of the start position column in `dat1`.

- end_col.1:

  Name of the end position column in `dat2`.

- chrom_col.2:

  Name of the chromosome column in `dat2`.

- start_col.2:

  Name of the start position column in `dat2`.

- end_col.2:

  Name of the end position column in `dat2`.

- return_merged:

  Whether to return an object with columns from `dat1` and `dat2`
  merged.

- unique_only:

  Only return unique rows.

- style:

  GRanges style (e.g. "NCBI, "UCSC") set by
  [seqlevelsStyle](https://rdrr.io/pkg/GenomeInfoDb/man/seqlevelsStyle.html).

- verbose:

  Print messages.

## Examples

``` r
if (FALSE) { # \dontrun{
dat1 <- echodata::BST1
dat2 <- echoannot::xgr_example
GenomicRanges::mcols(dat2) <- NULL

gr.hits <- echoannot::granges_overlap(dat1 = dat1, 
                                      dat2 = dat2, 
                                      chrom_col.1 = "CHR",
                                      start_col.1 = "POS")
} # }
```
