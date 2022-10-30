#' Get data
#' 
#' Download remote resources stored on GitHub Releases via \pkg{piggyback}.
#' @inheritParams echodata::get_data 
#' @keywords internal
#' @importFrom echodata get_data
#' @importFrom tools R_user_dir
get_data <- function(fname,
                     repo = "RajLabMSSM/echoannot",
                     save_dir = tools::R_user_dir("echoannot",
                                                  which="cache"),
                     overwrite = FALSE) { 
    echodata::get_data(fname = fname, 
                       repo = repo, 
                       save_dir = save_dir, 
                       overwrite = overwrite)
}
