# Get data

Download remote resources stored on GitHub Releases via piggyback.

## Usage

``` r
get_data(
  fname,
  repo = "RajLabMSSM/echoannot",
  save_dir = tools::R_user_dir("echoannot", which = "cache"),
  overwrite = FALSE
)
```

## Arguments

- fname:

  File name.

- repo:

  GitHub repository name.

- save_dir:

  Local directory to cache data in.

- overwrite:

  Should any local files of the same name be overwritten? default
  `TRUE`.
