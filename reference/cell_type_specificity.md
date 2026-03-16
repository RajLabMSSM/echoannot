# Get cell-type-specifity score for each cell type

Aggregate SNP overlap across various epigenomic datasets and then
identify the number of SNPs overlapping by each cell type

## Usage

``` r
cell_type_specificity(
  plot_dat,
  merged_DT,
  plot_specificity = TRUE,
  min_count = NULL,
  top_celltype_only = FALSE,
  label_yaxis = TRUE,
  y_lab = NULL,
  show_genes = FALSE,
  x_strip_angle = 40,
  show_plot = TRUE
)
```

## Arguments

- plot_specificity:

  Plot cell-type specificity scores for locus instead of raw assay
  overlap counts.
