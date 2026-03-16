# bulkATACseq peaks from human brain tissue

Each row represents an individual peak identified in the bulk ATAC-seq
data.

## Usage

``` r
get_CORCES2020_bulkATACseq_peaks()
```

## Source

[doi:10.1038/s41588-020-00721-x](https://doi.org/10.1038/s41588-020-00721-x)

## Details

Data originally from Corces et al. (bioRxiv)
([doi:10.1038/s41588-020-00721-x](https://doi.org/10.1038/s41588-020-00721-x)
), as of May 2020. Specifically: *STable2_Features_bulkATAC-seq_Peaks*

## See also

Other CORCES2020:
[`CORCES2020_get_ATAC_peak_overlap()`](https://rajlabmssm.github.io/echoannot/reference/CORCES2020_get_ATAC_peak_overlap.md),
[`CORCES2020_get_hichip_fithichip_overlap()`](https://rajlabmssm.github.io/echoannot/reference/CORCES2020_get_hichip_fithichip_overlap.md),
[`CORCES2020_prepare_bulkATAC_peak_overlap()`](https://rajlabmssm.github.io/echoannot/reference/CORCES2020_prepare_bulkATAC_peak_overlap.md),
[`CORCES2020_prepare_scATAC_peak_overlap()`](https://rajlabmssm.github.io/echoannot/reference/CORCES2020_prepare_scATAC_peak_overlap.md),
[`get_CORCES2020_cicero_coaccessibility()`](https://rajlabmssm.github.io/echoannot/reference/get_CORCES2020_cicero_coaccessibility.md),
[`get_CORCES2020_hichip_fithichip_loop_calls()`](https://rajlabmssm.github.io/echoannot/reference/get_CORCES2020_hichip_fithichip_loop_calls.md),
[`get_CORCES2020_scATACseq_celltype_peaks()`](https://rajlabmssm.github.io/echoannot/reference/get_CORCES2020_scATACseq_celltype_peaks.md),
[`get_CORCES2020_scATACseq_peaks()`](https://rajlabmssm.github.io/echoannot/reference/get_CORCES2020_scATACseq_peaks.md)

## Examples

``` r
if (FALSE) { # \dontrun{
dat <- readxl::read_excel(
    file.path(
        "~/Desktop/Fine_Mapping/echolocatoR",
        "annotations/Coceres_2020",
        "STable2_Features_bulkATAC-seq_Peaks.xlsx"
    ),
    skip = 18
)
CORCES2020_bulkATACseq_peaks <- data.table::data.table(dat)

#### piggyback ####
tmp <- file.path(tempdir(), "CORCES2020_bulkATACseq_peaks.tsv.gz")
data.table::fwrite(CORCES2020_bulkATACseq_peaks, tmp, sep = "\t")
piggyback::pb_upload(
    file = tmp,
    repo = "RajLabMSSM/echoannot"
)
} # }
```
