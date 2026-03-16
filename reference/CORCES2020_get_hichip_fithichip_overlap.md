# Get overlap between data table of SNPs and HiChIP_FitHiChIP coaccessibility anchors

Anchors are the genomic regions that have evidence of being functionally
connected to one another (coaccessible), e.g. enhancer-promoter
interactions.

## Usage

``` r
CORCES2020_get_hichip_fithichip_overlap(query_dat, verbose = TRUE)
```

## Source

[doi:10.1038/s41588-020-00721-x](https://doi.org/10.1038/s41588-020-00721-x)

## Arguments

- query_dat:

  Fine-mapping results.

- verbose:

  Print messages.

## See also

Other CORCES2020:
[`CORCES2020_get_ATAC_peak_overlap()`](https://rajlabmssm.github.io/echoannot/reference/CORCES2020_get_ATAC_peak_overlap.md),
[`CORCES2020_prepare_bulkATAC_peak_overlap()`](https://rajlabmssm.github.io/echoannot/reference/CORCES2020_prepare_bulkATAC_peak_overlap.md),
[`CORCES2020_prepare_scATAC_peak_overlap()`](https://rajlabmssm.github.io/echoannot/reference/CORCES2020_prepare_scATAC_peak_overlap.md),
[`get_CORCES2020_bulkATACseq_peaks()`](https://rajlabmssm.github.io/echoannot/reference/get_CORCES2020_bulkATACseq_peaks.md),
[`get_CORCES2020_cicero_coaccessibility()`](https://rajlabmssm.github.io/echoannot/reference/get_CORCES2020_cicero_coaccessibility.md),
[`get_CORCES2020_hichip_fithichip_loop_calls()`](https://rajlabmssm.github.io/echoannot/reference/get_CORCES2020_hichip_fithichip_loop_calls.md),
[`get_CORCES2020_scATACseq_celltype_peaks()`](https://rajlabmssm.github.io/echoannot/reference/get_CORCES2020_scATACseq_celltype_peaks.md),
[`get_CORCES2020_scATACseq_peaks()`](https://rajlabmssm.github.io/echoannot/reference/get_CORCES2020_scATACseq_peaks.md)
