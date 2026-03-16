# Plot any missense variants

Plot any missense variants in fine-mapped data.

## Usage

``` r
plot_missense(
  merged_DT,
  snp_filter = "Support>0",
  label_yaxis = FALSE,
  x_label = "UCS missense\nmutations",
  show.legend = TRUE,
  show_numbers = FALSE,
  show_plot = TRUE
)
```

## Arguments

- merged_DT:

  Merged fine-mapping results data from `echolocatoR::finemap_loci`.

- snp_filter:

  Filter to use apply to SNPs before plotting.

- label_yaxis:

  Whether to label the y-axis.

- x_label:

  x-axis title.

- show.legend:

  logical. Should this layer be included in the legends? `NA`, the
  default, includes if any aesthetics are mapped. `FALSE` never
  includes, and `TRUE` always includes. It can also be a named logical
  vector to finely select the aesthetics to display. To include legend
  keys for all levels, even when no data exists, use `TRUE`. If `NA`,
  all levels are shown in legend, but unobserved levels are omitted.

- show_numbers:

  Whether to plot the numeric values or not.

- show_plot:

  Show plot.

## See also

Other annotate:
[`annotate_missense()`](https://rajlabmssm.github.io/echoannot/reference/annotate_missense.md),
[`biomart_snp_info()`](https://rajlabmssm.github.io/echoannot/reference/biomart_snp_info.md),
[`biomart_snps_to_geneInfo()`](https://rajlabmssm.github.io/echoannot/reference/biomart_snps_to_geneInfo.md),
[`haplor_epigenetics_enrichment()`](https://rajlabmssm.github.io/echoannot/reference/haplor_epigenetics_enrichment.md),
[`haplor_epigenetics_summary()`](https://rajlabmssm.github.io/echoannot/reference/haplor_epigenetics_summary.md),
[`haplor_haploreg()`](https://rajlabmssm.github.io/echoannot/reference/haplor_haploreg.md),
[`haplor_regulomedb()`](https://rajlabmssm.github.io/echoannot/reference/haplor_regulomedb.md),
[`snps_by_mutation_type()`](https://rajlabmssm.github.io/echoannot/reference/snps_by_mutation_type.md)

## Examples

``` r
if (FALSE) { # \dontrun{
merged_DT <- echodata::get_Nalls2019_merged()
gg_missense <- plot_missense(
    merged_DT = merged_DT,
    snp_filter = "Support>0"
)
gg_missense <- plot_missense(
    merged_DT = merged_DT,
    snp_filter = "Consensus_SNP==TRUE"
)
} # }
```
