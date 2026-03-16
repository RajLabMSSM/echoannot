# Annotate merged fine-mapping results from all loci

Annotate fine-mapping results from echolocatoR across all loci.

## Usage

``` r
annotate_snps(
  dat,
  SNP_col = "SNP",
  haploreg_annotation = TRUE,
  regulomeDB_annotation = TRUE,
  biomart_annotation = TRUE,
  verbose = TRUE
)
```

## Arguments

- dat:

  Table containing at least a SNP column.

- SNP_col:

  Name of the column in `dat` that contain's each SNPs RSID.

- haploreg_annotation:

  Annotate SNPs with HaploReg (using `HaploR`).

- regulomeDB_annotation:

  Annotate SNPs with regulomeDB (using `HaploR`).

- biomart_annotation:

  Annotate SNPs with `biomart`.

- verbose:

  Print messages.

## Examples

``` r
if (FALSE) { # \dontrun{
dat <- echodata::BST1[Consensus_SNP==TRUE,]
dat_annot <- annotate_snps(dat = dat)
} # }
```
