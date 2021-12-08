#' get data via \pkg{piggyback}
#'
#' @keywords internal
#' @importFrom piggyback pb_download
get_data <- function(fname,
                     repo = "RajLabMSSM/echoannot",
                     overwrite = FALSE) {
    tmp <- file.path(tempdir(), fname)
    if (!file.exists(tmp)) {
        Sys.setenv("piggyback_cache_duration" = 10)
        piggyback::pb_download(
            file = fname,
            dest = tempdir(),
            repo = repo,
            overwrite = overwrite
        )
    }
    return(tmp)
}
