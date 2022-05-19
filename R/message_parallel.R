#' Send messages to console even from within parallel processes
#' @return A message
#' @keywords internal
message_parallel <- function(..., v=TRUE) {
    if(isTRUE(v)){
        system(sprintf('echo "%s"', paste0(..., collapse = "")))    
    }
}
