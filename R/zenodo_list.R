#' List Zenodo files
#' 
#' List all files available in a given Zenodo record.
#' Requires already having a Zenodo account set up and 
#' @param token Zenodo
#'  \href{https://zenodo.org/account/settings/applications/tokens/new/}{
#'  Personal access token}.
#' @param concept_doi Concept DOI of the Zenodo record.
#' @param sandbox Whether to use the Zenodo or Zenodo Sandbox API.
#' @param as_datatable Return file info as a \link[data.table]{data.table}.
#' @param verbose Print messages.
#' @source 
#' \href{https://cran.r-project.org/web/packages/zen4R/vignettes/zen4R.html}{
#' zen4R vignette}
#' \href{https://rpubs.com/antaldaniel/zenodo-sandbox-setup}{
#' zen4R vignette}
#' @returns Nested list of Zenodo file info.
#' 
#' @keywords internal
#' @importFrom data.table data.table rbindlist
zenodo_list <- function(token,
                        concept_doi="10.5281/zenodo.7062237",
                        sandbox=FALSE,
                        as_datatable=TRUE,
                        verbose=TRUE){  
    
    requireNamespace("zen4R")
    zenodo <- zen4R::ZenodoManager$new( 
        url = if(isTRUE(sandbox)) {
            "https://sandbox.zenodo.org/api"
        } else {
            "https://zenodo.org/api"
        },
        token = token, 
        logger = "INFO" 
    )  
    myrec <- zenodo$getDepositionByConceptDOI(concept_doi) 
    zen_files <- zenodo$getFiles(myrec$id)
    #### Report ####
    messager(formatC(length(zen_files),big.mark = ","),
            "files found.", v=verbose) 
    #### Return ####
    if(isTRUE(as_datatable)){ 
        zen_dat <- lapply(zen_files, function(x){
            data.table::data.table(
                filename=x$filename, 
                filesize=x$filesize,
                id=x$id, 
                download_link=x$links$download,
                self_link=x$links$self
            )
        }) |> data.table::rbindlist()
        split <- stringr::str_split(gsub("\\.bgz|\\.tbi|\\.l2",
                                         "",zen_dat$filename),
                                    "\\.|_",
                                    simplify = TRUE) 
        zen_dat$population <- split[,2]
        zen_dat$chrom <- as.integer(gsub("chr","",split[,3]))
        zen_dat$type <- split[,4]
        zen_dat[,url:=paste0("https://zenodo.org/record/7062238/files/",
                             filename,"?download=1")]
        return(zen_dat)
    } else {
        return(zen_files)
    } 
}
