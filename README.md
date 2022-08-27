<img src='https://github.com/RajLabMSSM/echoannot/raw/main/inst/hex/hex.png' height='300'><br><br>
[![](https://img.shields.io/badge/devel%20version-0.99.7-black.svg)](https://github.com/RajLabMSSM/echoannot)
[![R build
status](https://github.com/RajLabMSSM/echoannot/workflows/R-CMD-check-bioc/badge.svg)](https://github.com/RajLabMSSM/echoannot/actions)
[![](https://img.shields.io/github/last-commit/RajLabMSSM/echoannot.svg)](https://github.com/RajLabMSSM/echoannot/commits/main)
[![](https://app.codecov.io/gh/RajLabMSSM/echoannot/branch/main/graph/badge.svg)](https://app.codecov.io/gh/RajLabMSSM/echoannot)
[![License:
GPL-3](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://cran.r-project.org/web/licenses/GPL-3)
<h5>
Author: <i>Brian M. Schilder</i>
</h5>
<h5>
README updated: <i>Aug-27-2022</i>
</h5>

## `echoannot`: Functions for annotating genomic data

with annotations and epigenomic data.

This R package is part of the *echoverse* suite that supports
[`echolocatoR`](https://github.com/RajLabMSSM/echolocatoR): an automated
genomic fine-mapping pipeline.

If you use `echoannot`, please cite:

## Installation

``` r
if(!require("remotes")) install.packages("remotes")

remotes::install_github("RajLabMSSM/echoannot")
library(echoannot)
```

## Documentation

### [Website](https://rajlabmssm.github.io/echoannot)

### [Getting started](https://rajlabmssm.github.io/echoannot/articles/echoannot)

## Datasets

For more detailed information about each dataset, use `?`:  
`R   library(echolocatoR)   ?NOTT_2019.interactome # example dataset`

### Epigenomic & genome-wide annotations

#### [Nott et al. (2019)](https://science.sciencemag.org/content/366/6469/1134.abstract)

-   Data from this publication contains results from cell type-specific
    (neurons, oligodendrocytes, astrocytes, microglia, & peripheral
    myeloid cells) epigenomic assays (H3K27ac, ATAC, H3K4me3) from human
    brain tissue.

-   For detailed metadata, see:

    ``` r
    data("NOTT_2019.bigwig_metadata")
    ```

-   Built-in datasets:

    -   Enhancer/promoter coordinates (as *GenomicRanges*)

    ``` r
    data("NOTT_2019.interactome")
    # Examples of the data nested in "NOTT_2019.interactome" object:
    NOTT_2019.interactome$`Neuronal promoters`
    NOTT_2019.interactome$`Neuronal enhancers`
    NOTT_2019.interactome$`Microglia promoters`
    NOTT_2019.interactome$`Microglia enhancers`
    ...
    ...
    ```

    -   PLAC-seq enhancer-promoter interactome coordinates

    ``` r
    NOTT_2019.interactome$H3K4me3_around_TSS_annotated_pe
    NOTT_2019.interactome$`Microglia interactome`
    NOTT_2019.interactome$`Neuronal interactome`
    NOTT_2019.interactome$`Oligo interactome`
    ...
    ...
    ```

-   API access to full bigWig files on UCSC Genome Browser, which
    includes

    -   Epigenomic reads (as *GenomicRanges*)  
    -   Aggregate epigenomic *score* for each cell type - assay
        combination

#### [Corces et al. (2020)](https://www.biorxiv.org/content/10.1101/2020.01.06.896159v1)

-   Data from this preprint contains results from bulk and single-cell
    chromatin accessibility epigenomic assays in 39 human brains.

    ``` r
    data("CORCES_2020.bulkATACseq_peaks")
    data("CORCES_2020.cicero_coaccessibility")
    data("CORCES_2020.HiChIP_FitHiChIP_loop_calls")
    data("CORCES_2020.scATACseq_celltype_peaks")
    data("CORCES_2020.scATACseq_peaks")
    ```

#### [XGR](http://xgr.r-forge.r-project.org)

-   API access to a diverse library of cell type/line-specific
    epigenomic (e.g. ENCODE) and other genome-wide annotations.

#### [Roadmap](http://www.roadmapepigenomics.org)

-   API access to cell type-specific epigenomic data.

#### [biomaRt](https://bioconductor.org/packages/release/bioc/html/biomaRt.html)

-   API access to various genome-wide SNP annotations (e.g. missense,
    nonsynonmous, intronic, enhancer).

#### [HaploR](https://cran.r-project.org/web/packages/haploR/vignettes/haplor-vignette.html)

-   API access to known per-SNP QTL and epigenomic data hits.

<hr>

## Contact

<a href="https://bschilder.github.io/BMSchilder/" target="_blank">Brian
M. Schilder, Bioinformatician II</a>  
<a href="https://rajlab.org" target="_blank">Raj Lab</a>  
<a href="https://icahn.mssm.edu/about/departments/neuroscience" target="_blank">Department
of Neuroscience, Icahn School of Medicine at Mount Sinai</a>
