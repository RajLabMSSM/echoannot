# Bar plot of tool-specific CS sizes

Loci ordered by UCS size (smallest to largest).

## Usage

``` r
CS_counts_plot(
  merged_DT,
  show_numbers = TRUE,
  ylabel = "Locus",
  legend_nrow = 3,
  label_yaxis = TRUE,
  top_CS_only = FALSE,
  show_plot = TRUE
)
```

## Arguments

- merged_DT:

  Merged fine-mapping results data from `echolocatoR::finemap_loci`.

- show_numbers:

  Print numbers on top of bars.

- ylabel:

  y-axis label.

- legend_nrow:

  Number of rows for the legend to span over.

- label_yaxis:

  Whether or not to label the y-axis.

- top_CS_only:

  Only include the top 1 CS per fine-mapping method.

- show_plot:

  Show plot.

## See also

Other summarise:
[`CS_bin_plot()`](https://rajlabmssm.github.io/echoannot/reference/CS_bin_plot.md),
[`peak_overlap_plot()`](https://rajlabmssm.github.io/echoannot/reference/peak_overlap_plot.md),
[`plot_dataset_overlap()`](https://rajlabmssm.github.io/echoannot/reference/plot_dataset_overlap.md),
[`super_summary_plot()`](https://rajlabmssm.github.io/echoannot/reference/super_summary_plot.md)

## Examples

``` r
if (FALSE) { # \dontrun{
dat <- echodata::BST1
dat$Locus <- "BST1"
gg_CS <- echoannot::CS_counts_plot(merged_DT = dat)
} # }
```
