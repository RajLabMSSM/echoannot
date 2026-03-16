# Import bigwig files from the UCSC Genome Browser

Import, preprocess, merge, and save data from [UCSC Genome
Browser](https://genome.ucsc.edu/) tracks.

## Usage

``` r
import_ucsc_bigwigs(
  query_dat,
  bigwig_metadata,
  full_data = TRUE,
  xlims = NULL,
  save_path = tempfile(),
  force_new = FALSE,
  nThread = 1,
  verbose = TRUE
)
```

## Arguments

- query_dat:

  [data.table](https://rdrr.io/pkg/data.table/man/data.table.html) or
  [GRanges](https://rdrr.io/pkg/GenomicRanges/man/GRanges-class.html)
  containing genomic coordinates to query the UCSC tracks with.

- bigwig_metadata:

  Metadata table with at least the following two columns:

  "name"

  :   Unique name of the file.

  "data_link"

  :   URL to UCSC genome browser bigwig file.

- full_data:

  Whether to download the full data (genomic ranges of all sequence
  reads) as opposed to a reduced representation of the data as a single
  vector (i.e. the aggregated reads "score"). Setting `full_data=TRUE`
  is necessary for creating histograms and density plots.

- xlims:

  Min/max positions to filter from data after initial query. This helps
  to capture genomic ranges that only partially overlap with
  `query_dat`.

- save_path:

  Where to save the processed data.

- force_new:

  If a file already exists, download a new one anyway.

- nThread:

  Number of threads to parallelise downloads across.

- verbose:

  Print messages.

## Examples

``` r
if (FALSE) { # \dontrun{
bigwig_metadata <- echoannot::NOTT2019_bigwig_metadata[1,]
query_dat = echodata::BST1

bw.gr <- echoannot::import_ucsc_bigwigs(query_dat = query_dat, 
                                        bigwig_metadata = bigwig_metadata)
} # }
```
