# echoannot 0.99.7

## New features 

* `import_peaks_geo`:
    - Can now import/call peaks in parallel, 
    by splitting queries across chromosomes. 
    - Can now import >1 peak type at a time, 
    and documents it in "peaktype" col. 
* `import_peaks`: 
    - Pass up args: `regex_queries`, `split_chromosomes`
* New function: `process_ids`:
    - Able to get GSM sample names from GSE project ID.
* Split `import_peaks_geo` into separate`import_peaks_*` functions 
for each file type.
    
## Bug fixes 

* When only one peak type is returned from GEO search, 
prevent `mapply` from turning the results into a matrix with `SIMPLIFY=FALSE`. 
* Return empty `GenomicRanges::GRanges()` 
instead of error/NA/NULL during errors, so you can still merge at the end. 
    
    
# echoannot 0.99.6

## New features 

* `XGR_download_and_standardize`: Set `dat=NULL` to return genome-wide data.
* New vignette: *cell_type_specific_epigenomics*, 
    with Nott2019 and Corces2020 data.

## Bug fixes 

* `prepare_highlight_plac_data`: Allow to plot overlap with any SNP group 
(not just consensus SNPs).  

# echoannot 0.99.5

## New features 

* Moved `get_CS_counts`/`get_CS_bins` to `echodata`.   
* Cache remote data resources in `echoannot`-specific folder. 
* Add XGR GitHub installation again (CRAN installation not working again?). 
* Shorten Nott2019 ref links. 
* Pass all CRAN checks. 

## Bug fixes 

* Added `R.utils`/`tools` to *Imports*.  
* Added `GEOquery`/`regioneR`/
`BSgenome.Hsapiens.UCSC.hg38`/`BSgenome.Hsapiens.UCSC.hg19` to *Suggests*.  
* Updated GHA workflow to account for git security changes. 
* Fixed `convert_plots` when converting `Tracks` to list of ggplots.
* Fixed `merge_celltype_specific_epigenomics`
    - Added caching mechanism. 
* Use `echodata::<locus>` data instead of portal data for
*Getting started* vignette.
* Replace `file.path` with `paste`. 
* Replace "echoR" with "echoR_mini".
* Set timeout with multiple approaches: 
`options`,`httr::timeout`, `httr::config`. 

# echoannot 0.99.4

## New features 

* New functions
    - `import_peaks`: Search for peak files in GEO/ENCODE, or compute them 
    from bedGraph files. 
    - `call_peaks`: Call peaks from bedGraph files using `MACSr`. 
    - `test_enrichment`: Run permutation enrichment tests.

# echoannot 0.99.3

## Bug fixes 

* Update to match latest `echotabix` changes.
* Update GHA workflow.

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
