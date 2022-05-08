#' Get data 
#' 
#' Download remote resources stored on GitHub Releases via \pkg{piggyback}.
#'
#' @keywords internal
#' @importFrom piggyback pb_download
#' @importFrom tools R_user_dir
get_data <- function(fname,
                     repo = "RajLabMSSM/echoannot",
                     save_dir = tools::R_user_dir("echoannot",
                                                  which="cache"),
                     overwrite = FALSE) {
    
    dir.create(save_dir, showWarnings = FALSE, recursive = TRUE)
    tmp <- file.path(save_dir, fname)
    if (!file.exists(tmp)) {
        Sys.setenv("piggyback_cache_duration" = 10)
        piggyback::pb_download(
            file = fname,
            dest = save_dir,
            repo = repo,
            overwrite = overwrite
        )
    }
    return(tmp)
}
