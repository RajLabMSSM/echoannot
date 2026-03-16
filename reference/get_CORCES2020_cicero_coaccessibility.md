# Cicero_coaccessibility from human brain tissue

Cicero coaccessibility analysis for peaks that overlap SNPs derived from
analysis of scATAC-seq data. Each row represents an individual peak
identified from the feature binarization analysis (see methods).

## Usage

``` r
get_CORCES2020_cicero_coaccessibility()
```

## Source

[doi:10.1038/s41588-020-00721-x](https://doi.org/10.1038/s41588-020-00721-x)

## Details

Data originally from Corces et al. (bioRxiv)
([doi:10.1038/s41588-020-00721-x](https://doi.org/10.1038/s41588-020-00721-x)
), as of May 2020. Specifically:
*STable10_Coacessibility_Peak_loop_connection*, *Cicero Coaccessibility*
sheet. Peak_ID_Peak1 - A unique number that identifies the peak across
supplementary tables.

**Column dictionary**:

- hg38_Chromosome_Peak1:

  The hg38 chromosome of the first loop Peak.

- hg38_Start_Peak1:

  The hg38 start position of the first loop Peak.

- hg38_Stop_Peak1:

  The hg38 stop position of the first loop Peak.

- Width_Peak1:

  The width of the first loop Peak.

- Peak_ID_Peak2:

  A unique number that identifies the peak across supplementary tables.

- hg38_Chromosome_Peak2:

  The hg38 chromosome of the second loop Peak.

- hg38_Start_Peak2:

  The hg38 start position of the second loop Peak.

- hg38_Stop_Peak2:

  The hg38 stop position of the second loop Peak.

- Width_Peak2:

  The width of the second loop Peak.

- Coaccessibility:

  The coaccessibility correlation for the given peak pair.

- Peak1_hasSNP:

  A boolean variable determining whether the first peak overlaps a SNP
  from our AD/PD GWAS analyses.

- Peak2_hasSNP:

  A boolean variable determining whether the second peak overlaps a SNP
  from our AD/PD GWAS analyses.

## See also

Other CORCES2020:
[`CORCES2020_get_ATAC_peak_overlap()`](https://rajlabmssm.github.io/echoannot/reference/CORCES2020_get_ATAC_peak_overlap.md),
[`CORCES2020_get_hichip_fithichip_overlap()`](https://rajlabmssm.github.io/echoannot/reference/CORCES2020_get_hichip_fithichip_overlap.md),
[`CORCES2020_prepare_bulkATAC_peak_overlap()`](https://rajlabmssm.github.io/echoannot/reference/CORCES2020_prepare_bulkATAC_peak_overlap.md),
[`CORCES2020_prepare_scATAC_peak_overlap()`](https://rajlabmssm.github.io/echoannot/reference/CORCES2020_prepare_scATAC_peak_overlap.md),
[`get_CORCES2020_bulkATACseq_peaks()`](https://rajlabmssm.github.io/echoannot/reference/get_CORCES2020_bulkATACseq_peaks.md),
[`get_CORCES2020_hichip_fithichip_loop_calls()`](https://rajlabmssm.github.io/echoannot/reference/get_CORCES2020_hichip_fithichip_loop_calls.md),
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
    skip = 21, sheet = 2
)
CORCES2020_cicero_coaccessibility <- data.table::data.table(dat)

#### piggyback ####
tmp <- file.path(tempdir(), "CORCES2020_cicero_coaccessibility.tsv.gz")
data.table::fwrite(CORCES2020_cicero_coaccessibility, tmp, sep = "\t")
piggyback::pb_upload(
    file = tmp,
    repo = "RajLabMSSM/echoannot"
)
} # }
```
