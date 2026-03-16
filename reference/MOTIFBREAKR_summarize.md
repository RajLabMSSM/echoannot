# Summarize motifbreakR + echolocatoR results

Summarize motifbreakR + echolocatoR results after they have been merged
together with
[MOTIFBREAKR_filter](https://rajlabmssm.github.io/echoannot/reference/MOTIFBREAKR_filter.md).

## Usage

``` r
MOTIFBREAKR_summarize(mb_merge)
```

## Arguments

- mb_merge:

  Output of
  [MOTIFBREAKR_filter](https://rajlabmssm.github.io/echoannot/reference/MOTIFBREAKR_filter.md).

## Value

Per-locus summary in data.frame format.

## See also

Other motifbreakR:
[`MOTIFBREAKR()`](https://rajlabmssm.github.io/echoannot/reference/MOTIFBREAKR.md),
[`MOTIFBREAKR_filter_by_metadata()`](https://rajlabmssm.github.io/echoannot/reference/MOTIFBREAKR_filter_by_metadata.md)

## Examples

``` r
if (FALSE) { # \dontrun{
merged_DT <- echodata::get_Nalls2019_merged()
mb_res <- MOTIFBREAKR(rsid_list = c("rs11175620"),
                      # limit the number of datasets tests 
                      # for demonstration purposes only
                      pwmList_max = 4,
                      calculate_pvals = FALSE)
mb_merge <- MOTIFBREAKR_filter(mb_res = mb_res,
                               merged_DT = merged_DT,
                               pvalue_threshold = NULL)
summary_ls <- MOTIFBREAKR_summarize(mb_merge = mb_merge)                         
} # }
```
