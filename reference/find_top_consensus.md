# Find the top Consensus SNP

Identify the `n_top` Consensus SNP(s) per Locus, defined as the
Consensus SNPs with the highest mean PP across all fine-mapping tools
used.

## Usage

``` r
find_top_consensus(dat, n_top = 1, grouping_vars = c("Locus"))
```
