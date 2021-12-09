limits_from_ggplot <- function(gg,
                               variable = "x"){
    lim <- c(min(ggplot2::ggplot_build(gg)$data[[1]][variable],
                  na.rm = TRUE),
              max(ggplot2::ggplot_build(gg)$data[[1]][variable],
                  na.rm = TRUE)
             )
    return(lim)
}