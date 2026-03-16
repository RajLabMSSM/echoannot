# Plot inter-study SNP overlap

Cross-tabulate SNP overlap (after applying filter) between each pair of
studies.

## Usage

``` r
plot_dataset_overlap(
  merged_DT,
  snp_filter = "!is.na(SNP)",
  filename = NA,
  formula_str = "~ SNP + Dataset",
  triangle = FALSE
)
```

## Arguments

- merged_DT:

  Merged fine-mapping results data from `echolocatoR::finemap_loci`.

- snp_filter:

  Filter to use apply to SNPs before plotting.

- filename:

  Path to save file to as PNG.

- formula_str:

  Formula passed to [xtabs](https://rdrr.io/r/stats/xtabs.html).

- triangle:

  Plot correlation matrix as a square or a triangle.

## See also

Other summarise:
[`CS_bin_plot()`](https://rajlabmssm.github.io/echoannot/reference/CS_bin_plot.md),
[`CS_counts_plot()`](https://rajlabmssm.github.io/echoannot/reference/CS_counts_plot.md),
[`peak_overlap_plot()`](https://rajlabmssm.github.io/echoannot/reference/peak_overlap_plot.md),
[`super_summary_plot()`](https://rajlabmssm.github.io/echoannot/reference/super_summary_plot.md)

## Examples

``` r
if (FALSE) { # \dontrun{
    merged_DT <- echodata::get_Nalls2019_merged()
    merged_DT$Dataset <- rep(c("Dataset1","Dataset2"),nrow(merged_DT)/2)
    snp_xprod <- plot_dataset_overlap(merged_DT = merged_DT)
    
} # }
```
