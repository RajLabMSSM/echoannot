# Brain cell type-specific interactomes with superenhancers

Originally from Nott et al. (2019)
([doi:10.1126/science.aay0793](https://doi.org/10.1126/science.aay0793)
). Specifically: *aay0793-Nott-Table-S6.xlsx*.

## Usage

``` r
get_NOTT2019_superenhancer_interactome()
```

## Source

[doi:10.1126/science.aay0793](https://doi.org/10.1126/science.aay0793)

## See also

Other NOTT2019:
[`NOTT2019_bigwig_metadata`](https://rajlabmssm.github.io/echoannot/reference/NOTT2019_bigwig_metadata.md),
[`NOTT2019_epigenomic_histograms()`](https://rajlabmssm.github.io/echoannot/reference/NOTT2019_epigenomic_histograms.md),
[`NOTT2019_get_epigenomic_peaks()`](https://rajlabmssm.github.io/echoannot/reference/NOTT2019_get_epigenomic_peaks.md),
[`NOTT2019_get_interactions()`](https://rajlabmssm.github.io/echoannot/reference/NOTT2019_get_interactions.md),
[`NOTT2019_get_interactome()`](https://rajlabmssm.github.io/echoannot/reference/NOTT2019_get_interactome.md),
[`NOTT2019_get_promoter_celltypes()`](https://rajlabmssm.github.io/echoannot/reference/NOTT2019_get_promoter_celltypes.md),
[`NOTT2019_get_promoter_interactome_data()`](https://rajlabmssm.github.io/echoannot/reference/NOTT2019_get_promoter_interactome_data.md),
[`NOTT2019_get_regulatory_regions()`](https://rajlabmssm.github.io/echoannot/reference/NOTT2019_get_regulatory_regions.md),
[`NOTT2019_plac_seq_plot()`](https://rajlabmssm.github.io/echoannot/reference/NOTT2019_plac_seq_plot.md),
[`NOTT2019_superenhancers()`](https://rajlabmssm.github.io/echoannot/reference/NOTT2019_superenhancers.md),
[`get_NOTT2019_interactome()`](https://rajlabmssm.github.io/echoannot/reference/get_NOTT2019_interactome.md)

## Examples

``` r
if (FALSE) { # \dontrun{
NOTT2019_superenhancer_interactome <- data.table::data.table(
    readxl::read_excel(
        file.path(
            "~/Desktop/Fine_Mapping/echolocatoR",
            "annotations/NOTT2019/aay0793-Nott-Table-S6.xlsx"
        ),
        skip = 2
    )
)

#### piggyback ####
tmp <- file.path(tempdir(), "NOTT2019_superenhancer_interactome.tsv.gz")
data.table::fwrite(NOTT2019_superenhancer_interactome, tmp, sep = "\t")
piggyback::pb_upload(
    file = tmp,
    repo = "RajLabMSSM/echoannot"
)
} # }
```
