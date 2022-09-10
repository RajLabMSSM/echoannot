#' Process PAINTOR files
#' 
#' Process PAINTOR annotation/LD-score files and upload them to Zenodo.
#' @source 
#' \code{
#' #### Downloading annotations ####
#' ## Tarball be downloaded manually from here:
#' https://ucla.box.com/s/x47apvgv51au1rlmuat8m4zdjhcniv2d
#' ## Then decompress the tarball:
#' tar -xvf Functional_Annotations.tar.gz
#' }
#' @source 
#' \code{
#' #### Installing zen4R ####
#' ## has some system deps that have to be installed beforehand.
#' sudo apt-get install raptor2-utils
#' sudo apt-get install rasqal-utils
#' sudo apt-get install librdf0-dev
#' }
#' @param annot_dir Directory where \code{Functional_Annotations.tar.gz}
#'  data has been decompressed to. 
#' @inheritParams zenodo_list
#' 
#' @keywords internal
#' @importFrom echotabix convert
#' @importFrom stringr str_split
#' @importFrom stats setNames
#' @importFrom utils zip
PAINTOR_process <- function(annot_dir="./",
                            zipped_file="Functional_Annotations.zip",
                            sandbox=TRUE,
                            token){  
    
    requireNamespace("zen4R")
    # file_types <- c("\\.annot\\.gz$", "\\.ldscore.gz")
    all_files <- list.files("./",full.names = TRUE, recursive = TRUE)  
    # length(unique(basename(all_files)))==length(all_files)
    # tail(sort(table(basename(all_files))))
    #### Compress ####
    ## Files must be zipped to take advantage of Zenodo's 
    ## ability to show the directory structure of .zip files.
    out <- utils::zip(zipfile = zipped_file,
                      files = "../Functional_Annotations")
    
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
    newrec$setTitle(title = "PAINTOR")
    newrec$setDescription(description = 
    paste("Functional annotations used for fine-mapping with PAINTOR.", 
    "https://github.com/gkichaev/PAINTOR_V3.0/wiki/2b.-Overlapping-annotations",
    "These annotations are used by echolocatoR to perform",
    "in silico validation analyses.",
    "Data has been reprocessed to enable API access and rapid querying",
    "https://github.com/RajLabMSSM/echolocatoR"))
    newrec$setUploadType(uploadType = "dataset")
    newrec$addCreator(firstname = "Brian", lastname = "Schilder", 
                      affiliation = "Imperial College London") 
    newrec <- zenodo$depositRecord(newrec) 
    #### Upload files ####
    uploads <- lapply(stats::setNames(all_files,
                                      all_files),
                      function(x){
                          ### Go to sleep for 1h to reset limit of 5000 files/hour.
                          if(which(all_files==x) %in% seq(4999,4999*100,5000)) {
                              message("Waiting for 1 hour to reset upload limits.")
                              Sys.sleep(3600)  
                          }    
                          ### Go to sleep for 1min to reset limit of 100 files/min.
                          if(which(all_files==x) %in% seq(99,99*10000,100)) {
                              message("Waiting for 60 seconds to reset upload limits.")
                              Sys.sleep(60)  
                          }   
                          message("Uploading: ",x)
                          zenodo$uploadFile(x, record = newrec) 
                      })
    file_res <- zenodo$uploadFile(zipped_file, record = newrec) 
    #### Publish record ####
    options(timeout=3600*2)
    rec_res <- zenodo$publishRecord(recordId = newrec$id) 
    
    #### Validate everything was published #### 
    myrec <- zenodo$getDepositionByConceptDOI("10.5281/zenodo.7063240")
    # myrec <- zenodo$getDepositionByDOI("10.5281/zenodo.7063240")
    zen_files <- zenodo$getFiles(myrec$id) 
    # message(length(zen_files)," Zenodo files found.")  
    zfiles <- sapply(zen_files, function(x){x$filename})
    missing_files <- all_files[!basename(unlist(all_files)) %in% zfiles]
    if(length(missing_files)>0){
        wrn <- paste(length(missing_files),"missing files.")
        warning(wrn)
    }
    return(list(all_files=all_files,
                zipped_file=zipped_file))
}
