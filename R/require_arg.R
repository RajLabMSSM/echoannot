require_arg <- function(arg,
                        ls_out){
    if(!arg %in% ls_out) {
        stop_msg <- paste(arg,"must be supplied.")
        stop(stop_msg)
    }
}