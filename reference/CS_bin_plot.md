# Plot CS bin counts

Plot Credible Set bins counts for multiple fine-mapping method results.

## Usage

``` r
CS_bin_plot(merged_DT, show_plot = TRUE)
```

## Arguments

- merged_DT:

  Merged fine-mapping results data from `echolocatoR::finemap_loci`.

- show_plot:

  Show plot.

## See also

Other summarise:
[`CS_counts_plot()`](https://rajlabmssm.github.io/echoannot/reference/CS_counts_plot.md),
[`peak_overlap_plot()`](https://rajlabmssm.github.io/echoannot/reference/peak_overlap_plot.md),
[`plot_dataset_overlap()`](https://rajlabmssm.github.io/echoannot/reference/plot_dataset_overlap.md),
[`super_summary_plot()`](https://rajlabmssm.github.io/echoannot/reference/super_summary_plot.md)

## Examples

``` r
if (FALSE) { # \dontrun{
dat <- echodata::BST1 
dat$Locus <- "BST1"
bin_plot <- echoannot::CS_bin_plot(merged_DT = dat)
} # }
```
