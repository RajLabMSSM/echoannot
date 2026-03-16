# Gather Roadmap annotation metadata

Gather Roadmap annotation metadata

## Usage

``` r
ROADMAP_construct_reference(
  ref_path = system.file("extdata/ROADMAP", "ROADMAP_Epigenomic.js", package =
    "echoannot"),
  limit_files = NULL,
  keyword_query = NULL,
  verbose = TRUE
)
```

## Arguments

- ref_path:

  Where the ROADMAP metadata is stored.

- limit_files:

  Limit the number of annotation files queried (for faster testing).

- keyword_query:

  Search all columns in the Roadmap annotations metadata and only query
  annotations that contain your keywords. Can provide multiple keywords
  in list form: `c("placenta","liver","monocytes")`.

- verbose:

  Print messages.

## See also

Other ROADMAP:
[`ROADMAP_merge_and_process()`](https://rajlabmssm.github.io/echoannot/reference/ROADMAP_merge_and_process.md),
[`ROADMAP_query()`](https://rajlabmssm.github.io/echoannot/reference/ROADMAP_query.md),
[`ROADMAP_tabix()`](https://rajlabmssm.github.io/echoannot/reference/ROADMAP_tabix.md)

## Examples

``` r
if (FALSE) { # \dontrun{
ref <- ROADMAP_construct_reference(keyword_query = c(
    "placenta",
    "liver",
    "monocytes"))
} # }
```
