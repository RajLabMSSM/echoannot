# Plot brain cell-specific interactome data

Plot brain cell-specific interactome data from Nott et al. (2019)
([doi:10.1126/science.aay0793](https://doi.org/10.1126/science.aay0793)
).

## Usage

``` r
NOTT2019_plac_seq_plot(
  dat = NULL,
  locus_dir = NULL,
  title = NULL,
  show_plot = TRUE,
  save_plot = TRUE,
  return_interaction_track = FALSE,
  x_limits = NULL,
  zoom_window = NULL,
  index_SNP = NULL,
  genomic_units = "POS",
  color_dict = c(enhancers = "springgreen2", promoters = "purple", anchors = "black"),
  highlight_plac = TRUE,
  show_regulatory_rects = TRUE,
  show_anchors = TRUE,
  strip.text.y.angle = 0,
  xtext = TRUE,
  save_annot = FALSE,
  point_size = 2,
  height = 7,
  width = 7,
  dpi = 300,
  return_as = "Tracks",
  nThread = 1,
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

- locus_dir:

  Locus-specific directory.

- title:

  all title elements: plot, axes, legends
  ([`element_text()`](https://ggplot2.tidyverse.org/reference/element.html);
  inherits from `text`)

- show_plot:

  Print plot.

- save_plot:

  Whether to save the plot.

- return_interaction_track:

  Return only the interaction track (before completing the plot and
  showing it).

- x_limits:

  x-axis limits to be applied to all plots (useful when trying to keep a
  common coordinate system).

- zoom_window:

  Zoom window.

- index_SNP:

  Index/lead SNP RSID.

- genomic_units:

  Which genomic units to return window limits in.

- color_dict:

  Named list of colors for each regulatory element.

- highlight_plac:

  Whether to scale opacity of PLAC-seq interactions (arches) such that
  interactions with anchors containing Consensus SNPs will be colored
  darker (Default: `TRUE`). If `FALSE`, will instead apply the same
  opacity level to all interactions.

- show_regulatory_rects:

  Show enhancers/promoters as rectangles.

- show_anchors:

  Show PLAC-seq anchors.

- strip.text.y.angle:

  Angle of the y-axis facet labels.

- xtext:

  Whether to include x-axis title and text.

- save_annot:

  Save the queried subset of bigwig annotations.

- point_size:

  Point size of each SNP in the GWAS/fine-mapping plots.

- height:

  height (defaults to the height of current plotting window)

- width:

  width (defaults to the width of current plotting window)

- dpi:

  dpi to use for raster graphics

- return_as:

  Plot class to convert `plot_list` to:

  ggplot

  :   [ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html)

  ggbio

  :   [ggbio](https://rdrr.io/pkg/ggbio/man/ggbio-class.html)

  patchwork

  :   patchwork

  Tracks

  :   [tracks](https://rdrr.io/pkg/ggbio/man/tracks.html)

  NULL

  :   Return original object.

- nThread:

  Number of threads to parallelise downloads across.

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
[`NOTT2019_get_regulatory_regions()`](https://rajlabmssm.github.io/echoannot/reference/NOTT2019_get_regulatory_regions.md),
[`NOTT2019_superenhancers()`](https://rajlabmssm.github.io/echoannot/reference/NOTT2019_superenhancers.md),
[`get_NOTT2019_interactome()`](https://rajlabmssm.github.io/echoannot/reference/get_NOTT2019_interactome.md),
[`get_NOTT2019_superenhancer_interactome()`](https://rajlabmssm.github.io/echoannot/reference/get_NOTT2019_superenhancer_interactome.md)

## Examples

``` r
if (FALSE) { # \dontrun{
trks_plus_lines <- echoannot::NOTT2019_plac_seq_plot(dat = echodata::BST1) 
} # }
```
