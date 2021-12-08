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
