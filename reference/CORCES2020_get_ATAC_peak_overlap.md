# Get overlap between datatable of SNPs and scATAC peaks

Can optionally add `Cicero` coaccessibility scores, which are also
derived from scATAC-seq data.

## Usage

``` r
CORCES2020_get_ATAC_peak_overlap(
  query_dat,
  FDR_filter = NULL,
  add_cicero = TRUE,
  cell_type_specific = TRUE,
  verbose = TRUE
)
```

## Source

[doi:10.1038/s41588-020-00721-x](https://doi.org/10.1038/s41588-020-00721-x)

## Arguments

- query_dat:

  Genomic summary statistics data to query with.

- FDR_filter:

  Correct p-value threshold.

- add_cicero:

  Whether to include
  [cicero](https://www.bioconductor.org/packages/release/bioc/html/cicero.html)
  results as well.

- cell_type_specific:

  Whether to use bulk or cell-type-specific data.

- verbose:

  Print messages.

## See also

Other CORCES2020:
[`CORCES2020_get_hichip_fithichip_overlap()`](https://rajlabmssm.github.io/echoannot/reference/CORCES2020_get_hichip_fithichip_overlap.md),
[`CORCES2020_prepare_bulkATAC_peak_overlap()`](https://rajlabmssm.github.io/echoannot/reference/CORCES2020_prepare_bulkATAC_peak_overlap.md),
[`CORCES2020_prepare_scATAC_peak_overlap()`](https://rajlabmssm.github.io/echoannot/reference/CORCES2020_prepare_scATAC_peak_overlap.md),
[`get_CORCES2020_bulkATACseq_peaks()`](https://rajlabmssm.github.io/echoannot/reference/get_CORCES2020_bulkATACseq_peaks.md),
[`get_CORCES2020_cicero_coaccessibility()`](https://rajlabmssm.github.io/echoannot/reference/get_CORCES2020_cicero_coaccessibility.md),
[`get_CORCES2020_hichip_fithichip_loop_calls()`](https://rajlabmssm.github.io/echoannot/reference/get_CORCES2020_hichip_fithichip_loop_calls.md),
[`get_CORCES2020_scATACseq_celltype_peaks()`](https://rajlabmssm.github.io/echoannot/reference/get_CORCES2020_scATACseq_celltype_peaks.md),
[`get_CORCES2020_scATACseq_peaks()`](https://rajlabmssm.github.io/echoannot/reference/get_CORCES2020_scATACseq_peaks.md)

## Examples

``` r
if (FALSE) { # \dontrun{
query_dat <- echodata::BST1[1:100,]
gr.hits <- echoannot::CORCES2020_get_ATAC_peak_overlap(query_dat = query_dat)
} # }
```
