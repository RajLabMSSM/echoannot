# Query Roadmap API

Query Roadmap epigenomic annotations (chromatin marks) using a range of
genomic coordinates.

## Usage

``` r
ROADMAP_tabix(
  eid,
  query_dat,
  save_dir = tempdir(),
  force_new = FALSE,
  conda_env = "echoR_mini",
  nThread = 1,
  verbose = TRUE
)
```

## Source

` query_dat <- echodata::BST1 dat <- ROADMAP_tabix( query_dat = query_dat, eid = "E099") `

## Arguments

- eid:

  Roadmap annotation ID

- query_dat:

  Variant-level summary statistics.

- save_dir:

  Directory to store query results in.

- force_new:

  Overwrite any existing files of the same name.

- verbose:

  Print messages.

## Details

[ROADMAP file
locations.](https://egg2.wustl.edu/roadmap/data/byFileType/chromhmmSegmentations/ChmmModels/coreMarks/jointModel/final/)

## See also

Other ROADMAP:
[`ROADMAP_construct_reference()`](https://rajlabmssm.github.io/echoannot/reference/ROADMAP_construct_reference.md),
[`ROADMAP_merge_and_process()`](https://rajlabmssm.github.io/echoannot/reference/ROADMAP_merge_and_process.md),
[`ROADMAP_query()`](https://rajlabmssm.github.io/echoannot/reference/ROADMAP_query.md)
