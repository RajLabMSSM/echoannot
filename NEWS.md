# echoannot 0.99.3

## Bug fixes 

* Update to match latest `echotabix` changes.

# echoannot 0.99.3

## New features

* Added function `convert_plots`.

## Bug fixes 

* Move as many Imports to Suggests as possible. 
* GHA servers can't install XGR from CRAN. Switching to 
[GitHub remote](github::hfang-bristol/XGR) installation until they fix this. 
* Removed docs/ folder. 
* Reduced vignette size. 
* Transition all usage of `ggplot2` to `requireNamespace` method to reduce
NAMESPACE.
* Make all new col assignments in `GRanges` objects use the proper `mcols` syntax.
* Make sure all `GRanges` in UCSC format in `NOTT2019_plac_seq_plot`.
* Subdivide `NOTT2019_plac_seq_plot` into subfunctions. 
* New exports:
    - `NOTT2019_get_epigenomic_peaks`

# echoannot 0.99.2

## New features 

* Added a `NEWS.md` file to track changes to the package.
* Replace the following with `echoverseTemplate` versions:
    - New hex logo.
    - GHA
    - README
    - Vignette header
* Offload functions to `echodata`:
    - `assign_lead_snp`
    - `find_consensus_snps`
    - `update_cols`
    - `biomart_geneInfo`
    - `find_top_consensus`
    - `dt_to_granges`
    - `is_granges`
    - `granges_to_bed`
    - `reassign_lead_snps`
* Renamed all functions to exclude ".":
    - "ROADMAP." --> "ROADMAP_"
    - "NOTT_2019." --> "NOTT2019_"
    - etc.
* Added `xgr_query` example data. 
* Added unit tests for `super_summary_plot` and its subfunctions.
* Offloaded all liftover functions to `echotabix`
* Made new wrapper functions to simplify importing/plotting pipelines:
    - `XGR_plot`
    - `ROADMAP_plot`
