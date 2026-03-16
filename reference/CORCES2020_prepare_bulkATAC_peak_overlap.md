# Prepare data to plot overlap between datatable of SNPs and cell-type-specific epigenomic peaks and coaccessibility data.

Prepare data to plot overlap between datatable of SNPs and
cell-type-specific epigenomic peaks and coaccessibility data.

## Usage

``` r
CORCES2020_prepare_bulkATAC_peak_overlap(
  merged_DT,
  FDR_filter = NULL,
  snp_filter = "Consensus_SNP==TRUE",
  add_HiChIP_FitHiChIP = TRUE,
  annotate_genes = FALSE,
  return_counts = TRUE,
  verbose = TRUE
)
```

## Source

[doi:10.1038/s41588-020-00721-x](https://doi.org/10.1038/s41588-020-00721-x)

## See also

Other CORCES2020:
[`CORCES2020_get_ATAC_peak_overlap()`](https://rajlabmssm.github.io/echoannot/reference/CORCES2020_get_ATAC_peak_overlap.md),
[`CORCES2020_get_hichip_fithichip_overlap()`](https://rajlabmssm.github.io/echoannot/reference/CORCES2020_get_hichip_fithichip_overlap.md),
[`CORCES2020_prepare_scATAC_peak_overlap()`](https://rajlabmssm.github.io/echoannot/reference/CORCES2020_prepare_scATAC_peak_overlap.md),
[`get_CORCES2020_bulkATACseq_peaks()`](https://rajlabmssm.github.io/echoannot/reference/get_CORCES2020_bulkATACseq_peaks.md),
[`get_CORCES2020_cicero_coaccessibility()`](https://rajlabmssm.github.io/echoannot/reference/get_CORCES2020_cicero_coaccessibility.md),
[`get_CORCES2020_hichip_fithichip_loop_calls()`](https://rajlabmssm.github.io/echoannot/reference/get_CORCES2020_hichip_fithichip_loop_calls.md),
[`get_CORCES2020_scATACseq_celltype_peaks()`](https://rajlabmssm.github.io/echoannot/reference/get_CORCES2020_scATACseq_celltype_peaks.md),
[`get_CORCES2020_scATACseq_peaks()`](https://rajlabmssm.github.io/echoannot/reference/get_CORCES2020_scATACseq_peaks.md)

## Examples

``` r
if (FALSE) { # \dontrun{
dat_melt <- echoannot:::CORCES2020_prepare_bulkATAC_peak_overlap(
    merged_DT = echodata::get_Nalls2019_merged()
)
} # }
```
