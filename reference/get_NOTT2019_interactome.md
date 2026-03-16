# Brain cell type-specific enhancers, promoters, and interactomes

Originally from Nott et al. (2019)
([doi:10.1126/science.aay0793](https://doi.org/10.1126/science.aay0793)
). Specifically: *aay0793-Nott-Table-S5.xlsx*.

## Usage

``` r
get_NOTT2019_interactome()
```

## Source

[doi:10.1126/science.aay0793](https://doi.org/10.1126/science.aay0793)

` file <- file.path( "~/Desktop/Fine_Mapping/echolocatoR/annotations", "NOTT2019/aay0793-Nott-Table-S5.xlsx" ) sheets <- readxl::excel_sheets(file) enh_prom_sheets <- grep("enhancers|promoters", sheets, value = TRUE) other_sheets <- grep("enhancers|promoters", sheets, value = TRUE, invert = TRUE ) NOTT2019_interactome <- lapply(other_sheets, function(s) { readxl::read_excel(file, sheet = s, skip = 2) }) NOTT2019_interactome <- append( NOTT2019_interactome, lapply(enh_prom_sheets, function(s) { readxl::read_excel(file, sheet = s, skip = 2, col_names = c("chr", "start", "end") ) }) ) names(NOTT2019_interactome) <- c(other_sheets, enh_prom_sheets) #### piggyback #### tmp <- file.path(tempdir(), "NOTT2019_interactome.rds") saveRDS(NOTT2019_interactome, tmp) piggyback::pb_upload( file = tmp, repo = "RajLabMSSM/echoannot" ) `

` NOTT2019_interactome <- get_NOTT2019_interactome() `

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
[`NOTT2019_plac_seq_plot()`](https://rajlabmssm.github.io/echoannot/reference/NOTT2019_plac_seq_plot.md),
[`NOTT2019_superenhancers()`](https://rajlabmssm.github.io/echoannot/reference/NOTT2019_superenhancers.md),
[`get_NOTT2019_superenhancer_interactome()`](https://rajlabmssm.github.io/echoannot/reference/get_NOTT2019_superenhancer_interactome.md)
