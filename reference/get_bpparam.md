# Get [BiocParallel](https://rdrr.io/pkg/BiocParallel/man/BiocParallel-package.html) parameters

Get (and optionally register)
[BiocParallel](https://rdrr.io/pkg/BiocParallel/man/BiocParallel-package.html)
parameter (`BPPARAM`).
[SnowParam](https://rdrr.io/pkg/BiocParallel/man/SnowParam-class.html)
is the default function as it tends to be more robust. However, because
it doesn't work on Windows, this function automatically detected the
Operating System and switches to
[SerialParam](https://rdrr.io/pkg/BiocParallel/man/SerialParam-class.html)
as needed.

## Usage

``` r
get_bpparam(workers, register_now = FALSE)
```

## Arguments

- workers:

  Number of threads to parallelize across.

- register_now:

  Register the cores now with
  [register](https://rdrr.io/pkg/BiocParallel/man/register.html)
  (`TRUE`), or simply return the `BPPARAM object` (default: `FALSE`).

## Value

BPPARAM
