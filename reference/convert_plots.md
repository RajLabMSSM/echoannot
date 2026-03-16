# Convert plots to various formats

Takes a plot or just of plots and converts to various formats.

## Usage

``` r
convert_plots(
  plot_list,
  return_as = c("ggplot", "ggbio", "patchwork", "Tracks", NULL),
  x_limits = NULL,
  tag_levels = letters,
  params_list = NULL,
  verbose = TRUE
)
```

## Arguments

- plot_list:

  A plot, or a list of plots belonging to one of the following classes:

  - [ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html)

  - [ggbio](https://rdrr.io/pkg/ggbio/man/ggbio-class.html)

  - [tracks](https://rdrr.io/pkg/ggbio/man/tracks.html)

- return_as:

  Plot class to convert `plot_list` to:

  ggplot

  :   [ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html)

  ggbio

  :   [ggbio](https://rdrr.io/pkg/ggbio/man/ggbio-class.html)

  patchwork

  :   patchwork

  Tracks

  :   [tracks](https://rdrr.io/pkg/ggbio/man/tracks.html)

  NULL

  :   Return original object.

- x_limits:

  x-axis limits to be applied to all plots (useful when trying to keep a
  common coordinate system).

- tag_levels:

  A character vector defining the enumeration format to use at each
  level. Possible values are `'a'` for lowercase letters, `'A'` for
  uppercase letters, `'1'` for numbers, `'i'` for lowercase Roman
  numerals, and `'I'` for uppercase Roman numerals. It can also be a
  list containing character vectors defining arbitrary tag sequences. If
  any element in the list is a scalar and one of `'a'`, `'A'`, `'1'`,
  `'i`, or `'I'`, this level will be expanded to the expected sequence.

- params_list:

  A list of parameters to pass to
  [tracks](https://rdrr.io/pkg/ggbio/man/tracks.html).

- verbose:

  Print messages.

## Examples

``` r
if (FALSE) { # \dontrun{
#### Create example plot_list ####
gg <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, cyl)) + ggplot2::geom_point()
plot_list <- lapply(1:3, function(x) gg)

##### As ggplot #####
return_out1 <- convert_plots(plot_list = plot_list, return_as = "ggplot")
##### As ggbio #####
return_out2 <- convert_plots(plot_list = plot_list, return_as = "ggbio")
##### As patchwork #####
return_out3 <- convert_plots(plot_list = plot_list, return_as = "patchwork")
##### As Tracks #####
return_out4 <- convert_plots(plot_list = plot_list, return_as = "Tracks")
##### From Tracks #####
return_out5 <- echoannot::convert_plots(plot_list = return_out4)
} # }
```
