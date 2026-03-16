# Merge all summary plots into one super plot

Merge all summary plots into one super plot

## Usage

``` r
super_summary_plot(
  merged_DT,
  snp_filter = "Consensus_SNP==TRUE",
  coloc_results = NULL,
  credset_thresh = 0.8,
  plot_missense = TRUE,
  show_plot = TRUE,
  save_plot = FALSE,
  height = 15,
  width = 13,
  dpi = 300
)
```

## Arguments

- merged_DT:

  Merged fine-mapping results data from `echolocatoR::finemap_loci`.

- snp_filter:

  Filter to use apply to SNPs before plotting.

- coloc_results:

  Colocalization results from
  [catalogueR](https://github.com/RajLabMSSM/catalogueR).

- credset_thresh:

  The minimum mean Posterior Probability (across all fine-mapping
  methods used) of SNPs to be included in the "mean.CS" column.

- plot_missense:

  Whether to include the missense mutations plot. *Warning:* Can take a
  lot time to query Biomart.

- show_plot:

  Show plot.

- save_plot:

  Save plot.

- height:

  Plot height.

- width:

  Plot width.

- dpi:

  Plot resolution. Also accepts a string input: "retina" (320), "print"
  (300), or "screen" (72). Only applies when converting pixel units, as
  is typical for raster output types.

## See also

Other summarise:
[`CS_bin_plot()`](https://rajlabmssm.github.io/echoannot/reference/CS_bin_plot.md),
[`CS_counts_plot()`](https://rajlabmssm.github.io/echoannot/reference/CS_counts_plot.md),
[`peak_overlap_plot()`](https://rajlabmssm.github.io/echoannot/reference/peak_overlap_plot.md),
[`plot_dataset_overlap()`](https://rajlabmssm.github.io/echoannot/reference/plot_dataset_overlap.md)

## Examples

``` r
if (FALSE) { # \dontrun{
merged_DT <- echodata::get_Nalls2019_merged() 
super_plot <- echoannot::super_summary_plot(merged_DT = merged_DT,
                                            plot_missense = FALSE)
} # }
```
