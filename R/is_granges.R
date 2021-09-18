#' is_granges
#' 
#' @keywords internal
#' @importFrom methods is
is_granges <- function(obj){
    methods::is(obj,"GRanges")
}