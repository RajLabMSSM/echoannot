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
#' @inheritParams zenodo_list
#' @keywords internal
#' @importFrom echotabix convert
#' @importFrom stringr str_split
#' @importFrom stats setNames
#' @importFrom utils zip
IMPACT_process <- function(IMPACT_dir,
                           sandbox=TRUE,
                           token){  
    
    requireNamespace("zen4R")
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
    #### Compress ####
    out <- utils::zip(zipfile = "IMPACT707",
                      files = IMPACT_dir,
                      extras = list("-x *.gz"))
    
    #### Upload to zenodo ####
    zenodo <- zen4R::ZenodoManager$new(
        #### zenodo sandbox ####
        url = if(isTRUE(sandbox)) {
            "https://sandbox.zenodo.org/api"
        } else {
            "https://zenodo.org/api"
        },
        token = token, 
        ##  use "DEBUG" to see detailed API operation logs, 
        ## use NULL if you don't want logs at all
        logger = "INFO" 
    )   
    #### Create new record ####
    newrec <- zen4R::ZenodoRecord$new() 
    newrec$setTitle(title = "IMPACT")
    newrec$setDescription(description = 
  paste("Annotations produced by IMPACT",
        "(Inference and Modeling of Phenotype-related ACtive Transcription)",
        "to predict functional impact of genome sequence variation.",
        "Source: https://github.com/immunogenomics/IMPACT",
        "These annotations are used by echolocatoR to perform",
        "in silico validation analyses.",
        "Data has been reprocessed to enable API access and querying of",
        "files using tabix.\n",
        "https://github.com/RajLabMSSM/echolocatoR"))
    newrec$setUploadType(uploadType = "dataset")
    newrec$addCreator(firstname = "Brian", lastname = "Schilder", 
                      affiliation = "Imperial College London") 
    newrec <- zenodo$depositRecord(newrec) 
    #### Upload files ####
    uploads <- lapply(unlist(all_files), function(x){
        message("Uploading: ",x)
        zenodo$uploadFile(x, record = newrec) 
    })
    zipped_file <- "IMPACT707.zip"
    file_res <- zenodo$uploadFile(zipped_file, record = newrec) 
    #### Publish record ####
    rec_res <- zenodo$publishRecord(recordId = newrec$id) 
    #### Validate everything was published #### 
    myrec <- zenodo$getDepositionByConceptDOI("10.5281/zenodo.7062237")
    zen_files <- zenodo$getFiles(myrec$id)
    # message(length(zen_files)," Zenodo files found.")  
    zfiles <- sapply(zen_files, function(x){x$filename})
    all_there <- all(
        c(basename(unlist(all_files)),zipped_file) %in% zfiles
    )
    return(list(all_files=all_files,
                zipped_file=zipped_file))
}
