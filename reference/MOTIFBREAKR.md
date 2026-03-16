# Run motifbreakR

motifbreakR is a package to predict how much a SNP will disrupt a
transcription factor binding motif (if it falls within one). *Notes:*\

- BSgenome:

  Users must manually run
  [`library(BSgenome)`](https://bioconductor.org/packages/BSgenome)
  before running any motifbreakR functions to successfully use this
  tool.

- `threshold=`:

  If `filterp=TRUE`, this argument indicates the p-value threshold. If
  `filterp=FALSE`, this argument instead indicates the pct threshold.

## Usage

``` r
MOTIFBREAKR(
  rsid_list,
  results_dir = file.path(tempdir(), "results"),
  pwmList = NULL,
  pwmList_max = NULL,
  genome_build = NULL,
  organism = "Hsapiens",
  threshold = 0.85,
  show.neutral = FALSE,
  method = "default",
  calculate_pvals = TRUE,
  force_new = FALSE,
  background = c(A = 0.25, C = 0.25, G = 0.25, T = 0.25),
  granularity = NULL,
  nThread = 1,
  verbose = TRUE
)
```

## Source

[Publication](https://pubmed.ncbi.nlm.nih.gov/26272984/)
[GitHub](https://github.com/Simon-Coetzee/MotifBreakR)
[Vingette](http://simon-coetzee.github.io/motifBreakR)

## Arguments

- rsid_list:

  RSIDs of SNPs to test for motif disruption between the reference and
  alternative alleles..

- results_dir:

  Directory where results should be saved as a file named:
  *\<results_dir\>/\_genome_wide/motifbreakR/motifbreakR_results.rds*.
  If `NULL`, results will not be saved to disk.

- pwmList:

  An object of class `TFBSTools::MotifList` containing position weight
  matrices. If `NULL`, defaults to
  [`MotifDb::MotifDb`](https://rdrr.io/pkg/MotifDb/man/MotifDb.html).

- pwmList_max:

  Limit the maximum number of PWM datasets tested (e.g. `10`). If
  `NULL`, no limit it set.

- genome_build:

  Genome build to use.

- organism:

  Only include datasets in the `pwmList` performed in a particular
  organism.

- threshold:

  A numeric value used as a threshold for filtering results.

- show.neutral:

  Logical. Include neutral effects in results.

- method:

  Character string specifying the method for scoring effects.

- calculate_pvals:

  Calculate p-values for all SNPs tested. *WARNING:* May take a long
  time if many SNPs and/or PWM are selected.

- force_new:

  If results of the same name already exist, overwrite them with new
  analyses (`TRUE`). Otherwise, import the existing results and skip the
  analyses (default: `FALSE`).

- background:

  A named numeric vector of background nucleotide frequencies (A, C,
  G, T) summing to 1.

- granularity:

  Granularity for p-value calculation. If `NULL`, a default is chosen
  automatically.

- nThread:

  Number of threads to parallelize analyses across.

- verbose:

  Print messages.

## Value

Motif disruption predictions in
[GRanges](https://rdrr.io/pkg/GenomicRanges/man/GRanges-class.html)
format.

## See also

Other motifbreakR:
[`MOTIFBREAKR_filter_by_metadata()`](https://rajlabmssm.github.io/echoannot/reference/MOTIFBREAKR_filter_by_metadata.md),
[`MOTIFBREAKR_summarize()`](https://rajlabmssm.github.io/echoannot/reference/MOTIFBREAKR_summarize.md)

## Examples

``` r
if (FALSE) { # \dontrun{
library(BSgenome) ## <-- IMPORTANT!
#### Example fine-mapping results ####
merged_DT <- echodata::get_Nalls2019_merged()
#### Run motif analyses ####
mb_res <- MOTIFBREAKR(rsid_list = c("rs11175620"),
                      # limit the number of datasets tested
                      # for demonstration purposes only
                      pwmList_max = 4,
                      calculate_pvals = FALSE)
} # }
```
