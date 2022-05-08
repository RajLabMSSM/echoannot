#' Convert plots to various formats 
#' 
#' Takes a plot or just of plots and converts to various formats.
#' 
#' @param plot_list A plot, or a list of plots belonging
#'  to one of the following classes:
#' \itemize{
#' \item{\link[ggplot2]{ggplot}}
#' \item{\link[ggbio]{ggbio}}
#' \item{\link[ggbio]{tracks}}
#' }
#' @param return_as Plot class to convert \code{plot_list} to:
#' \itemize{
#' \item{"ggplot"}{\link[ggplot2]{ggplot}}
#' \item{"ggbio"}{\link[ggbio]{ggbio}}
#' \item{"patchwork"}{\pkg{patchwork}}
#' \item{"Tracks"}{\link[ggbio]{tracks}}
#' \item{NULL}{Return original object.}
#' }
#' @param x_limits x-axis limits to be applied to all plots
#'  (useful when trying to keep a common coordinate system).
#' @param params_list A list of parameters to pass to \link[ggbio]{tracks}.
#' @param verbose Print messages.
#' @inheritParams patchwork::plot_annotation
#' 
#' @export
#' @importFrom methods is
#' @importFrom patchwork wrap_plots plot_annotation
#' @importFrom ggbio ggbio
#' @importFrom ggplot2 xlim
#' @examples 
#' #### Create example plot_list ####
#' gg <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, cyl)) + ggplot2::geom_point()
#' plot_list <- lapply(1:3, function(x) gg)
#' 
#' ##### As ggplot #####
#' return_out1 <- convert_plots(plot_list = plot_list, return_as = "ggplot")
#' ##### As ggbio #####
#' return_out2 <- convert_plots(plot_list = plot_list, return_as = "ggbio")
#' ##### As patchwork #####
#' return_out3 <- convert_plots(plot_list = plot_list, return_as = "patchwork")
#' ##### As Tracks #####
#' return_out4 <- convert_plots(plot_list = plot_list, return_as = "Tracks")
#' ##### From Tracks #####
#' return_out5 <- echoannot::convert_plots(plot_list = return_out4)
convert_plots <- function(plot_list,
                          return_as = c("ggplot",
                                        "ggbio",
                                        "patchwork",
                                        "Tracks",
                                        NULL),
                          x_limits = NULL,
                          tag_levels = letters,
                          params_list = NULL,
                          verbose = TRUE){
    if(is.null(return_as)) return(plot_list)
    opts <- c("ggplot","ggbio","patchwork","Tracks") 
    if(methods::is(plot_list,"Tracks")){
        plot_list <- plot_list@plot
    }
    if(!is.list(plot_list)) {
        plot_list <- list("plot"=plot_list) 
    }
    #### Ensure plot_list has names ####
    if(is.null(names(plot_list))){
        messager("Automatically naming plot_list.",v=verbose)
        names(plot_list) <- paste0("plot",seq_len(length(plot_list)))
    }
    #### x_limits ####
    if(!is.null(x_limits)) {
        messager("x_limits will be used to limit the",
                 "min/max x-axis values for all plots.", v=verbose)
    } 
    #### Convert ####
    return_as <- tolower(return_as[1])
    #### ggplot ####
    if(return_as=="ggplot"){ 
        return(
            as_ggplot(plot_list = plot_list,
                      x_limits = x_limits, 
                      verbose = verbose) 
        )
    #### ggbio ####
    } else if (return_as=="ggbio"){
        return(
            as_ggbio(plot_list = plot_list,
                     x_limits = x_limits,
                     verbose = verbose) 
        )
    #### patchwork ####
    } else if (return_as=="patchwork"){
        return(
            as_patchwork(plot_list = plot_list,
                         ncol = 1,
                         tag_levels = tag_levels,
                         x_limits = x_limits,
                         verbose = verbose)
        )
    #### tracks ####
    } else if (return_as=="tracks"){
        return(
            as_tracks(plot_list = plot_list,
                      params_list = params_list,
                      x_limits = x_limits,
                      verbose = verbose) 
        )
    #### error ####
    } else {
        stop_msg <- paste("return_as must be one of:\n",
                          paste("-",opts, collapse = "\n "))
        stop(stop_msg)
    }
}
