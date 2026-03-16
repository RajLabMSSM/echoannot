# Plot enrichment results

Plot enrichment results from XGR annotations.

## Usage

``` r
XGR_enrichment_plot(
  enrich_res,
  title = NULL,
  subtitle = NULL,
  facet_formula = NULL,
  line_formula = "y ~ x",
  line_method = "lm",
  line_span = 1,
  FDR_thresh = 1,
  plot_type = "bar",
  shape_var = "Cell_type",
  facet_scales = "free",
  show_plot = TRUE,
  save_plot = FALSE,
  height = 5,
  width = 5,
  dpi = 300
)
```

## Arguments

- width, height:

  Plot size in units expressed by the `units` argument. If not supplied,
  uses the size of the current graphics device.

- dpi:

  Plot resolution. Also accepts a string input: "retina" (320), "print"
  (300), or "screen" (72). Only applies when converting pixel units, as
  is typical for raster output types.

## See also

Other XGR:
[`XGR_enrichment()`](https://rajlabmssm.github.io/echoannot/reference/XGR_enrichment.md),
[`XGR_enrichment_bootstrap()`](https://rajlabmssm.github.io/echoannot/reference/XGR_enrichment_bootstrap.md),
[`XGR_filter_assays()`](https://rajlabmssm.github.io/echoannot/reference/XGR_filter_assays.md),
[`XGR_filter_sources()`](https://rajlabmssm.github.io/echoannot/reference/XGR_filter_sources.md),
[`XGR_import_annotations()`](https://rajlabmssm.github.io/echoannot/reference/XGR_import_annotations.md),
[`XGR_iterate_enrichment()`](https://rajlabmssm.github.io/echoannot/reference/XGR_iterate_enrichment.md),
[`XGR_iterate_overlap()`](https://rajlabmssm.github.io/echoannot/reference/XGR_iterate_overlap.md),
[`XGR_merge_and_process()`](https://rajlabmssm.github.io/echoannot/reference/XGR_merge_and_process.md),
[`XGR_parse_metadata()`](https://rajlabmssm.github.io/echoannot/reference/XGR_parse_metadata.md),
[`XGR_plot_enrichment()`](https://rajlabmssm.github.io/echoannot/reference/XGR_plot_enrichment.md),
[`XGR_prepare_foreground_background()`](https://rajlabmssm.github.io/echoannot/reference/XGR_prepare_foreground_background.md),
[`XGR_query()`](https://rajlabmssm.github.io/echoannot/reference/xgr_query.md),
[`XGR_sep_handler()`](https://rajlabmssm.github.io/echoannot/reference/XGR_sep_handler.md),
[`xgr_example`](https://rajlabmssm.github.io/echoannot/reference/xgr_example.md)

## Examples

``` r
if (FALSE) { # \dontrun{
root <- file.path(
    "/sc/arion/projects/pd-omics/brian",
    "Fine_Mapping/Data/GWAS/Nalls23andMe_2019/_genome_wide"
)
### merged enrichment results
enrich_res <- data.table::fread(
    file.path(
        root,
        "XGR/celltypespecific_epigenomics.SNP_groups.csv.gz"
    )
)
enrich_res <- data.table::fread(
    file.path(
        root,
        "XGR/celltypespecific_epigenomics.snp_groups.csv.gz"
    )
)
enrich_boot <- data.table::fread(
    file.path(
        root,
        "XGR/celltypespecific_epigenomics.snp_groups.permute.csv.gz"
    )
)
enrich_assay <- data.table::fread(
    file.path(
        root,
        "XGR/celltypespecific_epigenomics.snp_groups.assay.csv.gz"
    )
)

# Merged volcano plot
enrich_res <- subset(enrich_res, SNP_Group != "Consensus (-PolyFun)") |>
    dplyr::rename(SNP_group = SNP_Group)
gp <- XGR_enrichment_plot(
    enrich_res = subset(enrich_res, !Assay %in% c("HiChIP_FitHiChIP", "PLAC")),
    title = "Enrichment: Cell-type-specific epigenomics",
    plot_type = "point",
    save_plot = file.path(
        root, "XGR/celltypespecific_epigenomics.enrich_volcano.png"
    ),
    height = 6, width = 8, shape_var = "Assay"
)
## Merged bar plot
gp <- XGR_enrichment_plot(
    enrich_res = enrich_res,
    plot_type = "bar",
    facet_formula = ".~Assay",
    FDR_thresh = .05
)
# Merged volcano plot (permuted)
gp <- XGR_enrichment_plot(
    enrich_res = enrich.scATAC.permute,
    title = "Permuted enrichment: Cell-type-specific peaks and elements",
    plot_type = "point"
)
} # }
```
