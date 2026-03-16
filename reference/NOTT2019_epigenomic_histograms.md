# Plot brain cell-specific epigenomic data

Brain cell-specific epigenomic data from Nott et al. (2019).

## Usage

``` r
NOTT2019_epigenomic_histograms(
  dat,
  bigwig_metadata = echoannot::NOTT2019_bigwig_metadata,
  locus_dir = tempdir(),
  show_plot = TRUE,
  save_plot = FALSE,
  full_data = TRUE,
  return_assay_track = FALSE,
  binwidth = 200,
  density_adjust = 0.2,
  zoom = "1x",
  strip.text.y.angle = 90,
  xtext = TRUE,
  geom = "density",
  plot_formula = "Cell_type ~.",
  fill_var = "Assay",
  genomic_units = "Mb",
  as_ggplot = TRUE,
  dpi = 300,
  height = 15,
  width = 8,
  nThread = 1,
  save_annot = FALSE,
  verbose = TRUE
)
```

## Source

Nott et al. (2019)
([doi:10.1126/science.aay0793](https://doi.org/10.1126/science.aay0793)
)

## Arguments

- dat:

  Fine-mapping results data from `echolocatoR::finemap_loci`.

- bigwig_metadata:

  Metadata table with at least the following two columns:

  "name"

  :   Unique name of the file.

  "data_link"

  :   URL to UCSC genome browser bigwig file.

- locus_dir:

  Locus-specific directory.

- show_plot:

  Show plot.

- save_plot:

  Whether to save the plot.

- full_data:

  Whether to download the full data (genomic ranges of all sequence
  reads) as opposed to a reduced representation of the data as a single
  vector (i.e. the aggregated reads "score"). Setting `full_data=TRUE`
  is necessary for creating histograms and density plots.

- return_assay_track:

  Return only the assay track (before adding the rest of the tracks and
  showing the plot).

- binwidth:

  width of the bins.

- density_adjust:

  Passed to `adjust` argument in
  [geom_density](https://ggplot2.tidyverse.org/reference/geom_density.html).

- zoom:

  Zoom into the center of the locus when plotting (without editing the
  fine-mapping results file). You can provide either:

  - The size of your plot window in terms of basepairs (e.g.
    `zoom=50000` for a 50kb window).

  - How much you want to zoom in (e.g. `zoom="1x"` for the full locus,
    `zoom="2x"` for 2x zoom into the center of the locus, etc.).

  You can pass a list of window sizes (e.g. `c(50000,100000,500000)`) to
  automatically generate multiple views of each locus. This can even be
  a mix of different style inputs: e.g. `c("1x","4.5x",25000)`.

- strip.text.y.angle:

  Angle of the y-axis facet labels.

- xtext:

  Whether to include x-axis title and text.

- geom:

  defaults for geoms
  ([`element_geom()`](https://ggplot2.tidyverse.org/reference/element.html))

- plot_formula:

  Formula passed to `facets` argument in
  [facet_grid](https://ggplot2.tidyverse.org/reference/facet_grid.html).

- fill_var:

  Variable name to use for plot `fill` argument.

- genomic_units:

  Which genomic units to return window limits in.

- as_ggplot:

  Return plot as `ggplot2` (`TRUE`) or `Tracks` (`FALSE`) object.

- dpi:

  dpi to use for raster graphics

- height:

  height (defaults to the height of current plotting window)

- width:

  width (defaults to the width of current plotting window)

- nThread:

  Number of threads to parallelise downloads across.

- save_annot:

  Save the queried subset of bigwig annotations.

- verbose:

  Print messages.

## See also

Other NOTT2019:
[`NOTT2019_bigwig_metadata`](https://rajlabmssm.github.io/echoannot/reference/NOTT2019_bigwig_metadata.md),
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

## Examples

``` r
if (FALSE) { # \dontrun{
nott2019_track <- echoannot::NOTT2019_epigenomic_histograms(
    dat = echodata::BST1, 
    bigwig_metadata = echoannot::NOTT2019_bigwig_metadata[1:2,])
} # }
```
