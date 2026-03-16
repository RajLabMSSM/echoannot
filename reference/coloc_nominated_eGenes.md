# Nominate target genes within each locus

Across all GWAS-QTL colocalization tests across all studies, take the
eGene with the highest colocalziation probability (PP.H4) and assign it
as the most likely causal gene in that locus.

## Usage

``` r
coloc_nominated_egenes(
  coloc_results,
  merged_DT,
  label_yaxis = TRUE,
  y_lab = "Locus",
  x_lab = NULL,
  fill_var = "PP.H4",
  text_size = 2,
  credset_thresh = NULL,
  nThread = 1,
  show_plot = TRUE,
  verbose = TRUE
)
```

## Arguments

- credset_thresh:

  The minimum mean Posterior Probability (across all fine-mapping
  methods used) of SNPs to be included in the "mean.CS" column.

- verbose:

  Print messages.

## Details

eQTL queries and colocalization test done with catalogueR.

## Examples

``` r
if (FALSE) { # \dontrun{
merged_DT <- echodata::get_Nalls2019_merged()
base_url <- "~/Desktop/Fine_Mapping/Data/GWAS/Nalls23andMe_2019"
coloc_results_path <- file.path(
    base_url, "_genome_wide/COLOC/coloc.eQTL_Catalogue_ALL.csv.gz"
)
gg_egene <- coloc_nominated_egenes(coloc_results,
    merged_DT = merged_DT,
    fill_var = NULL
)

# QTL
base_url <- "/sc/hydra/projects/ad-omics/microglia_omics/Fine_Mapping"
coloc_results_path <- file.path(
    base_url,
    "Kunkle_Microglia_all_regions/QTL_merged_coloc_results.snp.tsv.gz"
)
merged_DT <- data.table::fread(
    file.path(
        "/pd-omics/brian/Fine_Mapping/Data/QTL",
        "Microglia_all_regions",
        "multiGWAS.microgliaQTL_finemapping.csv.gz"
    )
)
gg_egene <- coloc_nominated_egenes(coloc_results,
    merged_DT = merged_DT,
    fill_var = NULL
)
} # }
```
