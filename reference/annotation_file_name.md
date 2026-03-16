# Annotation file name

Construct an annotation-specific file name.

## Usage

``` r
annotation_file_name(locus_dir, lib_name, suffix = ".rds")
```

## Arguments

- locus_dir:

  Locus-specific directory.

- lib_name:

  Annotation library name.

- suffix:

  File suffix.

## Examples

``` r
if (FALSE) { # \dontrun{
tmp <- tempdir()
annot_file <- annotation_file_name(locus_dir=tmp,
                                   lib_name="test")
} # }
```
