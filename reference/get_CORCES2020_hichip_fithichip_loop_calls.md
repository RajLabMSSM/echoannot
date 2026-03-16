# FitHiChIP loop calls from human brain tissue

FitHiChIP loop calls that overlap SNPs derived from analysis of H3K27ac
HiChIP data. Each row represents an individual peak identified from the
feature binarization analysis (see methods).

## Usage

``` r
get_CORCES2020_hichip_fithichip_loop_calls()
```

## Source

[doi:10.1038/s41588-020-00721-x](https://doi.org/10.1038/s41588-020-00721-x)

## Details

Data originally from Corces et al. (bioRxiv)
([doi:10.1038/s41588-020-00721-x](https://doi.org/10.1038/s41588-020-00721-x)
), as of May 2020. Specifically:
*STable10_Coacessibility_Peak_loop_connection*, *HiChIP FitHiChIP Loop
Calls* sheet.

**Column dictionary**

- hg38_Chromosome_Anchor1:

  The hg38 chromosome of the first loop Anchor.

- hg38_Start_Anchor1:

  The hg38 start position of the first loop Anchor.

- hg38_Stop_Anchor1:

  The hg38 stop position of the first loop Anchor.

- Width_Anchor1:

  The width of the first loop Anchor.

- hg38_Chromosome_Anchor2:

  The hg38 chromosome of the second loop Anchor.

- hg38_Start_Anchor2:

  The hg38 start position of the second loop Anchor.

- hg38_Stop_Anchor2:

  The hg38 stop position of the second loop Anchor.

- Width_Anchor2:

  The width of the second loop Anchor.

- Score:

  The -log10(q-value) of the loop call from FitHiChIP.

- Anchor1_hasSNP:

  A boolean variable determining whether the first anchor overlaps a SNP
  from our AD/PD GWAS analyses.

- Anchor2_hasSNP:

  A boolean variable determining whether the second anchor overlaps a
  SNP from our AD/PD GWAS analyses.

## See also

Other CORCES2020:
[`CORCES2020_get_ATAC_peak_overlap()`](https://rajlabmssm.github.io/echoannot/reference/CORCES2020_get_ATAC_peak_overlap.md),
[`CORCES2020_get_hichip_fithichip_overlap()`](https://rajlabmssm.github.io/echoannot/reference/CORCES2020_get_hichip_fithichip_overlap.md),
[`CORCES2020_prepare_bulkATAC_peak_overlap()`](https://rajlabmssm.github.io/echoannot/reference/CORCES2020_prepare_bulkATAC_peak_overlap.md),
[`CORCES2020_prepare_scATAC_peak_overlap()`](https://rajlabmssm.github.io/echoannot/reference/CORCES2020_prepare_scATAC_peak_overlap.md),
[`get_CORCES2020_bulkATACseq_peaks()`](https://rajlabmssm.github.io/echoannot/reference/get_CORCES2020_bulkATACseq_peaks.md),
[`get_CORCES2020_cicero_coaccessibility()`](https://rajlabmssm.github.io/echoannot/reference/get_CORCES2020_cicero_coaccessibility.md),
[`get_CORCES2020_scATACseq_celltype_peaks()`](https://rajlabmssm.github.io/echoannot/reference/get_CORCES2020_scATACseq_celltype_peaks.md),
[`get_CORCES2020_scATACseq_peaks()`](https://rajlabmssm.github.io/echoannot/reference/get_CORCES2020_scATACseq_peaks.md)

## Examples

``` r
if (FALSE) { # \dontrun{
dat <- readxl::read_excel(
    file.path(
        "~/Desktop/Fine_Mapping/echolocatoR/annotations",
        "Coceres_2020/STable10_Coacessibility_Peak_loop_connection.xlsx"
    ),
    skip = 19, sheet = 1
)
CORCES2020_hichip_fithichip_loop_calls <- data.table::data.table(dat)

#### piggyback ####
tmp <- file.path(tempdir(), "CORCES2020_hichip_fithichip_loop_calls.tsv.gz")
data.table::fwrite(CORCES2020_hichip_fithichip_loop_calls, tmp, sep = "\t")
piggyback::pb_upload(
    file = tmp,
    repo = "RajLabMSSM/echoannot"
)
} # }
```
