# Download cell type-specific epigenomic peaks

API access to brain cell type-specific epigenomic peaks (bed format)
from Nott et al. (2019).

## Usage

``` r
NOTT2019_get_epigenomic_peaks(
  assays = c("ATAC", "H3K27ac", "H3K4me3"),
  cell_types = c("neurons", "microglia", "oligo", "astrocytes"),
  convert_to_granges = TRUE,
  save_dir = tools::R_user_dir(package = "echoannot", which = "cache"),
  force_new = FALSE,
  nThread = 1,
  verbose = TRUE
)
```

## Source

Nott et al. (2019)
([doi:10.1126/science.aay0793](https://doi.org/10.1126/science.aay0793)
)

## Arguments

- assays:

  Which epigenomic assays to import data from.

- cell_types:

  Which cell-types to import data from.

- convert_to_granges:

  Whether to convert the peaks to a
  [GRanges](https://rdrr.io/pkg/GenomicRanges/man/GRanges-class.html)
  object.

- save_dir:

  Where to save the processed data.

- force_new:

  If the saved data already exists, re-downloaded anyway.

- nThread:

  Number of threads to parallelise downloads across.

- verbose:

  Print messages.

## See also

Other NOTT2019:
[`NOTT2019_bigwig_metadata`](https://rajlabmssm.github.io/echoannot/reference/NOTT2019_bigwig_metadata.md),
[`NOTT2019_epigenomic_histograms()`](https://rajlabmssm.github.io/echoannot/reference/NOTT2019_epigenomic_histograms.md),
[`NOTT2019_get_interactions()`](https://rajlabmssm.github.io/echoannot/reference/NOTT2019_get_interactions.md),
[`NOTT2019_get_interactome()`](https://rajlabmssm.github.io/echoannot/reference/NOTT2019_get_interactome.md),
[`NOTT2019_get_promoter_celltypes()`](https://rajlabmssm.github.io/echoannot/reference/NOTT2019_get_promoter_celltypes.md),
[`NOTT2019_get_promoter_interactome_data()`](https://rajlabmssm.github.io/echoannot/reference/NOTT2019_get_promoter_interactome_data.md),
[`NOTT2019_get_regulatory_regions()`](https://rajlabmssm.github.io/echoannot/reference/NOTT2019_get_regulatory_regions.md),
[`NOTT2019_plac_seq_plot()`](https://rajlabmssm.github.io/echoannot/reference/NOTT2019_plac_seq_plot.md),
[`NOTT2019_superenhancers()`](https://rajlabmssm.github.io/echoannot/reference/NOTT2019_superenhancers.md),
[`get_NOTT2019_interactome()`](https://rajlabmssm.github.io/echoannot/reference/get_NOTT2019_interactome.md),
[`get_NOTT2019_superenhancer_interactome()`](https://rajlabmssm.github.io/echoannot/reference/get_NOTT2019_superenhancer_interactome.md)

## Examples

``` r
if (FALSE) { # \dontrun{
PEAKS <- echoannot::NOTT2019_get_epigenomic_peaks() 
} # }
```
