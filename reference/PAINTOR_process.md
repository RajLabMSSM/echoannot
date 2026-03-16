# Process PAINTOR files

Process PAINTOR annotation/LD-score files and upload them to Zenodo.

## Usage

``` r
PAINTOR_process(
  annot_dir = "./",
  zipfile = "Functional_Annotations",
  sandbox = TRUE,
  title = "PAINTOR",
  token = Sys.getenv("zenodo_token"),
  validate = TRUE,
  verbose = TRUE
)
```

## Source

` #### Downloading annotations #### ## Tarball be downloaded manually from here: https://ucla.box.com/s/x47apvgv51au1rlmuat8m4zdjhcniv2d ## Then decompress the tarball: tar -xvf Functional_Annotations.tar.gz `

` #### Installing zen4R #### ## has some system deps that have to be installed beforehand. sudo apt-get install raptor2-utils sudo apt-get install rasqal-utils sudo apt-get install librdf0-dev `

## Arguments

- annot_dir:

  Directory where `Functional_Annotations.tar.gz` data has been
  decompressed to.

- sandbox:

  Whether to use the Zenodo or Zenodo Sandbox API.

- token:

  Zenodo [Personal access
  token](https://zenodo.org/account/settings/applications/tokens/new/).

- verbose:

  Print messages.
