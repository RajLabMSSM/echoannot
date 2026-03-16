# Test for enrichment of `HaploR` annotations

Test for enrichment of `HaploR` annotations

## Usage

``` r
haplor_epigenetics_enrichment(
  snp_list1,
  snp_list2,
  chisq = TRUE,
  fisher = TRUE,
  epigenetic_variables = c("Promoter_histone_marks", "Enhancer_histone_marks"),
  tissue_list = c("BRN", "BLD")
)
```

## Source

[HaploR](https://cran.r-project.org/web/packages/haploR/vignettes/haplor-vignette.html)

## See also

Other annotate:
[`annotate_missense()`](https://rajlabmssm.github.io/echoannot/reference/annotate_missense.md),
[`biomart_snp_info()`](https://rajlabmssm.github.io/echoannot/reference/biomart_snp_info.md),
[`biomart_snps_to_geneInfo()`](https://rajlabmssm.github.io/echoannot/reference/biomart_snps_to_geneInfo.md),
[`haplor_epigenetics_summary()`](https://rajlabmssm.github.io/echoannot/reference/haplor_epigenetics_summary.md),
[`haplor_haploreg()`](https://rajlabmssm.github.io/echoannot/reference/haplor_haploreg.md),
[`haplor_regulomedb()`](https://rajlabmssm.github.io/echoannot/reference/haplor_regulomedb.md),
[`plot_missense()`](https://rajlabmssm.github.io/echoannot/reference/plot_missense.md),
[`snps_by_mutation_type()`](https://rajlabmssm.github.io/echoannot/reference/snps_by_mutation_type.md)
