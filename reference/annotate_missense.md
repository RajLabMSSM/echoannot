# Annotate any missense variants

Annotate any missense variants with
[getBM](https://huber-group-embl.github.io/biomaRt/reference/getBM.html).

## Usage

``` r
annotate_missense(merged_DT, snp_filter = "Support>0")
```

## Arguments

- merged_DT:

  Merged fine-mapping results data from `echolocatoR::finemap_loci`.

- snp_filter:

  Row-wise filter to apply to `merged_DT` filter (provided as a string).

## See also

Other annotate:
[`biomart_snp_info()`](https://rajlabmssm.github.io/echoannot/reference/biomart_snp_info.md),
[`biomart_snps_to_geneInfo()`](https://rajlabmssm.github.io/echoannot/reference/biomart_snps_to_geneInfo.md),
[`haplor_epigenetics_enrichment()`](https://rajlabmssm.github.io/echoannot/reference/haplor_epigenetics_enrichment.md),
[`haplor_epigenetics_summary()`](https://rajlabmssm.github.io/echoannot/reference/haplor_epigenetics_summary.md),
[`haplor_haploreg()`](https://rajlabmssm.github.io/echoannot/reference/haplor_haploreg.md),
[`haplor_regulomedb()`](https://rajlabmssm.github.io/echoannot/reference/haplor_regulomedb.md),
[`plot_missense()`](https://rajlabmssm.github.io/echoannot/reference/plot_missense.md),
[`snps_by_mutation_type()`](https://rajlabmssm.github.io/echoannot/reference/snps_by_mutation_type.md)

## Examples

``` r
if (FALSE) { # \dontrun{
merged_DT <- echodata::get_Nalls2019_merged()
annotated_DT <- annotate_missense(merged_DT = merged_DT)
} # }
```
