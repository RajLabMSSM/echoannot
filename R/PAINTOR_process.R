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
#' @inheritParams downloadR::zenodo_list
#' 
#' @keywords internal 
#' @importFrom downloadR zenodo_upload
#' @importFrom utils zip
PAINTOR_process <- function(annot_dir="./",
                            zipfile="Functional_Annotations",
                            sandbox=TRUE,
                            title="PAINTOR", 
                            token=Sys.getenv("zenodo_token"),
                            validate=TRUE,
                            verbose=TRUE){   
    
    # file_types <- c("\\.annot\\.gz$", "\\.ldscore.gz")
    all_files <- list.files("./",full.names = TRUE, recursive = TRUE)  
    # length(unique(basename(all_files)))==length(all_files)
    # tail(sort(table(basename(all_files))))
    #### Compress ####
    ## Files must be zipped to take advantage of Zenodo's 
    ## ability to show the directory structure of .zip files.
    out <- utils::zip(zipfile = zipfile,
                      files = "../Functional_Annotations")
    
    meta <- list(
        description=
    paste("Functional annotations used for fine-mapping with PAINTOR.", 
  "https://github.com/gkichaev/PAINTOR_V3.0/wiki/2b.-Overlapping-annotations",
          "These annotations are used by echolocatoR to perform",
          "in silico validation analyses.",
          "Data has been reprocessed to enable API access and rapid querying",
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
