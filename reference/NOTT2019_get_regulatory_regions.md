# Get regulatory regions: Nott2019

Get epigenomic peak ranges from Nott2019.

## Usage

``` r
NOTT2019_get_regulatory_regions(
  as_granges = FALSE,
  nThread = 1,
  verbose = TRUE
)
```

## Source

Nott et al. (2019)
([doi:10.1126/science.aay0793](https://doi.org/10.1126/science.aay0793)
)

## Arguments

- as_granges:

  Return results a
  [GRanges](https://rdrr.io/pkg/GenomicRanges/man/GRanges-class.html)
  object instead of a
  [data.table](https://rdrr.io/pkg/data.table/man/data.table.html).

- nThread:

  Number of threads to parallelize across.

- verbose:

  Print messages.

## See also

Other NOTT2019:
[`NOTT2019_bigwig_metadata`](https://rajlabmssm.github.io/echoannot/reference/NOTT2019_bigwig_metadata.md),
[`NOTT2019_epigenomic_histograms()`](https://rajlabmssm.github.io/echoannot/reference/NOTT2019_epigenomic_histograms.md),
[`NOTT2019_get_epigenomic_peaks()`](https://rajlabmssm.github.io/echoannot/reference/NOTT2019_get_epigenomic_peaks.md),
[`NOTT2019_get_interactions()`](https://rajlabmssm.github.io/echoannot/reference/NOTT2019_get_interactions.md),
[`NOTT2019_get_interactome()`](https://rajlabmssm.github.io/echoannot/reference/NOTT2019_get_interactome.md),
[`NOTT2019_get_promoter_celltypes()`](https://rajlabmssm.github.io/echoannot/reference/NOTT2019_get_promoter_celltypes.md),
[`NOTT2019_get_promoter_interactome_data()`](https://rajlabmssm.github.io/echoannot/reference/NOTT2019_get_promoter_interactome_data.md),
[`NOTT2019_plac_seq_plot()`](https://rajlabmssm.github.io/echoannot/reference/NOTT2019_plac_seq_plot.md),
[`NOTT2019_superenhancers()`](https://rajlabmssm.github.io/echoannot/reference/NOTT2019_superenhancers.md),
[`get_NOTT2019_interactome()`](https://rajlabmssm.github.io/echoannot/reference/get_NOTT2019_interactome.md),
[`get_NOTT2019_superenhancer_interactome()`](https://rajlabmssm.github.io/echoannot/reference/get_NOTT2019_superenhancer_interactome.md)

## Examples

``` r
if (FALSE) { # \dontrun{
regions <- NOTT2019_get_regulatory_regions()
} # }
```
