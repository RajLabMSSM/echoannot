#' Process IMPACT files
#' 
#' Process IMPACT files so that they're all tabix-indexed
#' and prepare them for upload. 
#' @source 
#' \code{
#' #### Downloading annotations ####
#' git clone IMPACT
#' Mac:
#' git lfs fetch --all
#' git lfs pull
#' 
#' Linux:
#' sudo apt-get install git-lfs
#' git-lfs fetch --all
#' git-lfs pull 
#' }
#' @source 
#' \code{
#' #### Installing zen4R ####
#' ## has some system deps that have to be installed beforehand.
#' sudo apt-get install raptor2-utils
#' sudo apt-get install rasqal-utils
#' sudo apt-get install librdf0-dev
#' }
#' @param IMPACT_dir Directory where IMPACT repo has been cloned to.
#' @inheritParams downloadR::zenodo_list
#' @keywords internal
#' @importFrom downloadR zenodo_upload
#' @importFrom echotabix convert
#' @importFrom stringr str_split
#' @importFrom stats setNames 
IMPACT_process <- function(IMPACT_dir,
                           sandbox=TRUE,
                           title="IMPACT",
                           zipfile=file.path(tempdir(),"IMPACT707"),
                           token=Sys.getenv("zenodo_token"),
                           validate=TRUE,
                           verbose=TRUE){  
    
    file_types <- c("\\.annot\\.gz$", "\\.ldscore.gz")
    names(file_types) <- stringr::str_split(file_types,"\\.",
                                            simplify = TRUE)[,2]
    all_files <- lapply(stats::setNames(file_types,
                                        c("annot","ldscore")),
    function(pattern){ 
        cat("Processing:",pattern,"\n")                    
        files <- list.files(file.path(IMPACT_dir,"IMPACT707"),
                            pattern = pattern,
                            full.names = TRUE, recursive = TRUE)
        names(files) <- sapply(files,function(x){basename(x$path)})
        lapply(files, function(x){
            message("Processing: ",x) 
            echotabix::convert(target_path = x,
                               chrom_col = "CHR",
                               start_col = "BP",
                               comment_char = "CHR",
                               force_new = FALSE)
        })
    })  
    meta <- list(
        description=
            paste("Annotations produced by IMPACT",
                  "(Inference and Modeling of",
                  "Phenotype-related ACtive Transcription)",
                  "to predict functional impact of genome sequence variation.",
                  "Source: https://github.com/immunogenomics/IMPACT",
                  "These annotations are used by echolocatoR to perform",
                  "in silico validation analyses.",
                  "Data has been reprocessed to enable API access",
                  "and querying of files using tabix.\n",
                  "https://github.com/RajLabMSSM/echolocatoR"),
        uploadType="dataset",
        Creator=list(
            firstname = "Brian", 
            lastname = "Schilder", 
            affiliation = "Imperial College London"
        )
        )
    zout <- downloadR::zenodo_upload(files=all_files,
                                     token=token,
                                     title=title,
                                     zipfile=zipfile,
                                     meta=meta,
                                     sandbox=sandbox,
                                     validate=validate,
                                     verbose=verbose) 
    return(zout)
}
