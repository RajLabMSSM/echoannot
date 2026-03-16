# Plot overlap between some SNP group and various epigenomic data

Plot overlap between some SNP group and various epigenomic data

## Usage

``` r
peak_overlap_plot(
  merged_DT,
  snp_filter = "Consensus_SNP==TRUE",
  force_new = FALSE,
  include.NOTT2019_peaks = TRUE,
  include.NOTT2019_enhancers_promoters = TRUE,
  include.NOTT2019_PLACseq = TRUE,
  include.CORCES2020_scATACpeaks = TRUE,
  include.CORCES2020_Cicero_coaccess = TRUE,
  include.CORCES2020_bulkATACpeaks = TRUE,
  include.CORCES2020_HiChIP_FitHiChIP_coaccess = TRUE,
  include.CORCES2020_gene_annotations = TRUE,
  plot_celltype_specificity = TRUE,
  plot_celltype_specificity_genes = FALSE,
  facets_formula = ". ~ Cell_type",
  show_plot = TRUE,
  label_yaxis = TRUE,
  x_strip_angle = 90,
  x_tick_angle = 40,
  drop_empty_cols = FALSE,
  fill_title = paste(snp_filter, "\nin epigenomic peaks"),
  save_path = FALSE,
  height = 11,
  width = 12,
  subplot_widths = c(1, 0.5),
  verbose = TRUE
)
```

## Source

Nott et al., 2019 (The Lancet Neurology)
([doi:10.1126/science.aay0793](https://doi.org/10.1126/science.aay0793)
) Corces et al., 2020 (Nature Genetics)
([doi:10.1038/s41588-020-00721-x](https://doi.org/10.1038/s41588-020-00721-x)
)

## Arguments

- force_new:

  Don't use previously downloaded files.

- include.NOTT2019_peaks:

  Plot SNP subset overlap with peaks from cell-type-specific bulk ATAC,
  H3K27ac, and H3K4me3 assays.

- include.NOTT2019_enhancers_promoters:

  Plot SNP subset overlap with cell enhancers and promoters.

- include.CORCES2020_scATACpeaks:

  Plot SNP subset overlap with cell-type-specific scATAC-seq peaks.

- include.CORCES2020_Cicero_coaccess:

  Plot SNP subset overlap with Cicero coaccessibility peaks (derived
  from scATACseq).

## See also

Other summarise:
[`CS_bin_plot()`](https://rajlabmssm.github.io/echoannot/reference/CS_bin_plot.md),
[`CS_counts_plot()`](https://rajlabmssm.github.io/echoannot/reference/CS_counts_plot.md),
[`plot_dataset_overlap()`](https://rajlabmssm.github.io/echoannot/reference/plot_dataset_overlap.md),
[`super_summary_plot()`](https://rajlabmssm.github.io/echoannot/reference/super_summary_plot.md)

## Examples
