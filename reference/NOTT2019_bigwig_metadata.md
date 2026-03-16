# Metadata and links to data

Metadata for cell type-specific epigenomic bigWig files hosted on UCSC
Genome Browser. bigWig files contain the genomic ranges from each
epigenomic assay, as well as a Score column which describes the peaks of
the aggregate reads.

## Usage

``` r
data("NOTT2019_bigwig_metadata")
```

## Format

An object of class `data.table` (inherits from `data.frame`) with 18
rows and 14 columns.

## Source

[doi:10.1126/science.aay0793](https://doi.org/10.1126/science.aay0793)

` NOTT2019_bigwig_metadata <- data.table::data.table( readxl::read_excel( file.path( "~/Desktop/Fine_Mapping/echolocatoR/annotations", "NOTT2019/NOTT2019_snEpigenomics.xlsx" ) ) ) usethis::use_data(NOTT2019_bigwig_metadata, overwrite = TRUE) `

## See also

Other NOTT2019:
[`NOTT2019_epigenomic_histograms()`](https://rajlabmssm.github.io/echoannot/reference/NOTT2019_epigenomic_histograms.md),
[`NOTT2019_get_epigenomic_peaks()`](https://rajlabmssm.github.io/echoannot/reference/NOTT2019_get_epigenomic_peaks.md),
[`NOTT2019_get_interactions()`](https://rajlabmssm.github.io/echoannot/reference/NOTT2019_get_interactions.md),
[`NOTT2019_get_interactome()`](https://rajlabmssm.github.io/echoannot/reference/NOTT2019_get_interactome.md),
[`NOTT2019_get_promoter_celltypes()`](https://rajlabmssm.github.io/echoannot/reference/NOTT2019_get_promoter_celltypes.md),
[`NOTT2019_get_promoter_interactome_data()`](https://rajlabmssm.github.io/echoannot/reference/NOTT2019_get_promoter_interactome_data.md),
[`NOTT2019_get_regulatory_regions()`](https://rajlabmssm.github.io/echoannot/reference/NOTT2019_get_regulatory_regions.md),
[`NOTT2019_plac_seq_plot()`](https://rajlabmssm.github.io/echoannot/reference/NOTT2019_plac_seq_plot.md),
[`NOTT2019_superenhancers()`](https://rajlabmssm.github.io/echoannot/reference/NOTT2019_superenhancers.md),
[`get_NOTT2019_interactome()`](https://rajlabmssm.github.io/echoannot/reference/get_NOTT2019_interactome.md),
[`get_NOTT2019_superenhancer_interactome()`](https://rajlabmssm.github.io/echoannot/reference/get_NOTT2019_superenhancer_interactome.md)
