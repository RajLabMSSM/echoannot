# Test enrichment

Conduct permutation enrichment tests between all combinations of two
named lists, each containing one or more
[GRanges](https://rdrr.io/pkg/GenomicRanges/man/GRanges-class.html)
objects. Permutation tests are run using
[overlapPermTest](https://rdrr.io/pkg/regioneR/man/overlapPermTest.html).

## Usage

``` r
test_enrichment(
  grlist1,
  grlist2,
  ntimes = 100,
  genome = "hg19",
  alternative = "auto",
  min.parallel = 1000,
  force.parallel = NULL,
  seed = 2022,
  mc.set.seed = FALSE,
  save_path = tempfile(fileext = "_test_enrichment.rds"),
  verbose = TRUE,
  ...
)
```

## Source

[See section "3.7A note on reproducibility" for info on setting the
seed.](https://www.bioconductor.org/packages/devel/bioc/vignettes/regioneR/inst/doc/regioneR.html)

## Arguments

- grlist1:

  First list of
  [GRanges](https://rdrr.io/pkg/GenomicRanges/man/GRanges-class.html)
  objects.

- grlist2:

  Second list of
  [GRanges](https://rdrr.io/pkg/GenomicRanges/man/GRanges-class.html)
  objects.

- ntimes:

  number of permutations

- genome:

  The reference genome to use. A valid genome object. Either a
  GenomicRanges or data.frame containing one region per whole chromosome
  or a character uniquely identifying a genome in BSgenome (e.g. "hg19",
  "mm10" but not "hg"). Internally it uses getGenomeAndMask.

- alternative:

  the alternative hypothesis must be one of `"greater"`, `"less"` or
  `"auto"`. If `"auto"`, the alternative will be decided depending on
  the data.

- min.parallel:

  if force.parallel is not specified, this will be used to determine the
  threshold for parallel computation. If
  `length(A) * ntimes > min.parallel`, it will activate the parallel
  computation. Single threaded otherwise.

- force.parallel:

  logical indicating if the computation must be paralelized.

- seed:

  Set the seed for reproducibility.

- mc.set.seed:

  "In order to create reproducible code with functions that use random
  numbers such as the permutation testing in regioneR , it is necessary
  to use set.seed. However, since regioneR uses parallel to perform the
  test it is also necessary to set the `mc.set.seed` parameter to FALSE
  to ensure reproducibility."

- save_path:

  Path to save results to as an *RDS* file. Set to `NULL` to skip this
  step.

- verbose:

  Print messages.

- ...:

  Arguments passed on to
  [`regioneR::overlapPermTest`](https://rdrr.io/pkg/regioneR/man/overlapPermTest.html)

  `A`

  :   a region set in any of the accepted formats by
      [`toGRanges`](https://rdrr.io/pkg/regioneR/man/toGRanges.html)
      ([`GenomicRanges`](https://rdrr.io/pkg/GenomicRanges/man/GRanges-class.html),
      [`data.frame`](https://rdrr.io/r/base/data.frame.html), etc...)

  `B`

  :   a region set in any of the accepted formats by
      [`toGRanges`](https://rdrr.io/pkg/regioneR/man/toGRanges.html)
      ([`GenomicRanges`](https://rdrr.io/pkg/GenomicRanges/man/GRanges-class.html),
      [`data.frame`](https://rdrr.io/r/base/data.frame.html), etc...)

## Value

data.frame

## Examples

``` r
if (FALSE) { # \dontrun{
dat <- echodata::get_Nalls2019_merged() 
grlist1 <- dat[P<5e-8,]
grlist2 <- dat[Support>0,] 
enrich <- test_enrichment(grlist1 = grlist1,
                          grlist2 = grlist2,  
                          ntimes = 25,
                          force.parallel = FALSE) 
} # }
```
