# Download SNP-wise annotations from Biomart

Download SNP-wise annotations from Biomart

## Usage

``` r
biomart_snp_info(
  snp_list,
  reference_genome = "grch37",
  attributes = c("refsnp_id", "allele", "chr_name", "chrom_start", "chrom_end",
    "chrom_strand", "ensembl_gene_stable_id", "consequence_type_tv",
    "polyphen_prediction", "polyphen_score", "sift_prediction", "sift_score",
    "reg_consequence_types", "validated"),
  verbose = TRUE
)
```

## Source

[biomaRt](https://bioconductor.org/packages/release/bioc/html/biomaRt.html)

## See also

Other annotate:
[`annotate_missense()`](https://rajlabmssm.github.io/echoannot/reference/annotate_missense.md),
[`biomart_snps_to_geneInfo()`](https://rajlabmssm.github.io/echoannot/reference/biomart_snps_to_geneInfo.md),
[`haplor_epigenetics_enrichment()`](https://rajlabmssm.github.io/echoannot/reference/haplor_epigenetics_enrichment.md),
[`haplor_epigenetics_summary()`](https://rajlabmssm.github.io/echoannot/reference/haplor_epigenetics_summary.md),
[`haplor_haploreg()`](https://rajlabmssm.github.io/echoannot/reference/haplor_haploreg.md),
[`haplor_regulomedb()`](https://rajlabmssm.github.io/echoannot/reference/haplor_regulomedb.md),
[`plot_missense()`](https://rajlabmssm.github.io/echoannot/reference/plot_missense.md),
[`snps_by_mutation_type()`](https://rajlabmssm.github.io/echoannot/reference/snps_by_mutation_type.md)
