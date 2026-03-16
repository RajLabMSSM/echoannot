# Get overlap between SNPs and epigenomic peaks

Get overlap between SNPs and epigenomic peaks

## Usage

``` r
peak_overlap(
  merged_DT,
  snp_filter = "!is.na(SNP)",
  include.NOTT2019_peaks = TRUE,
  include.NOTT2019_enhancers_promoters = TRUE,
  include.NOTT2019_PLACseq = TRUE,
  include.CORCES2020_scATACpeaks = TRUE,
  include.CORCES2020_Cicero_coaccess = TRUE,
  include.CORCES2020_bulkATACpeaks = TRUE,
  include.CORCES2020_HiChIP_FitHiChIP_coaccess = TRUE,
  include.CORCES2020_gene_annotations = TRUE,
  verbose = T
)
```
