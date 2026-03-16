# Get cell type-specific promoter/emhancer/interactome data

Brain cell-specific epigenomic data from Nott et al. (2019).

## Usage

``` r
NOTT2019_get_promoter_interactome_data(dat = NULL)
```

## Source

Nott et al. (2019)
([doi:10.1126/science.aay0793](https://doi.org/10.1126/science.aay0793)
)

## Arguments

- dat:

  data.table of genomic coordinates to query with. Set as `NULL` to
  return genome-wide data.

## See also

Other NOTT2019:
[`NOTT2019_bigwig_metadata`](https://rajlabmssm.github.io/echoannot/reference/NOTT2019_bigwig_metadata.md),
[`NOTT2019_epigenomic_histograms()`](https://rajlabmssm.github.io/echoannot/reference/NOTT2019_epigenomic_histograms.md),
[`NOTT2019_get_epigenomic_peaks()`](https://rajlabmssm.github.io/echoannot/reference/NOTT2019_get_epigenomic_peaks.md),
[`NOTT2019_get_interactions()`](https://rajlabmssm.github.io/echoannot/reference/NOTT2019_get_interactions.md),
[`NOTT2019_get_interactome()`](https://rajlabmssm.github.io/echoannot/reference/NOTT2019_get_interactome.md),
[`NOTT2019_get_promoter_celltypes()`](https://rajlabmssm.github.io/echoannot/reference/NOTT2019_get_promoter_celltypes.md),
[`NOTT2019_get_regulatory_regions()`](https://rajlabmssm.github.io/echoannot/reference/NOTT2019_get_regulatory_regions.md),
[`NOTT2019_plac_seq_plot()`](https://rajlabmssm.github.io/echoannot/reference/NOTT2019_plac_seq_plot.md),
[`NOTT2019_superenhancers()`](https://rajlabmssm.github.io/echoannot/reference/NOTT2019_superenhancers.md),
[`get_NOTT2019_interactome()`](https://rajlabmssm.github.io/echoannot/reference/get_NOTT2019_interactome.md),
[`get_NOTT2019_superenhancer_interactome()`](https://rajlabmssm.github.io/echoannot/reference/get_NOTT2019_superenhancer_interactome.md)

## Examples

``` r
if (FALSE) { # \dontrun{
dat <- echodata::BST1
annot_sub <- NOTT2019_get_promoter_interactome_data(dat=dat)
} # }
```
