# Bind GRanges with different mcols

Bind GRanges with different mcols

## Usage

``` r
rbind_granges(gr1, gr2)
```

## Examples

``` r
if (FALSE) { # \dontrun{
merged_DT <- echodata::get_Nalls2019_merged()
gr.hits <- CORCES2020_get_ATAC_peak_overlap(dat = merged_DT)
gr.hits$extra_col <- "Extra"
gr.anchor_hits <- CORCES2020_get_hichip_fithichip_overlap(
    dat = merged_DT)
try({gr.bind <- c(gr.hits, gr.anchor_hits)})
gr.bound <- rbind_granges(gr1, gr2)
} # }
```
