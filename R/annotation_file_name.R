#' Annotation file name
#' 
#' Construct an annotation-specific file name.
#' @param locus_dir Locus-specific directory. 
#' @param lib_name Annotation library name.
#' @param suffix File suffix.
#' @export
#' @examples 
#' tmp <- tempdir()
#' annot_file <- annotation_file_name(locus_dir=tmp,
#'                                    lib_name="test")
annotation_file_name <- function(locus_dir,
                                 lib_name,
                                 suffix=".rds") {
    annot_dir <- file.path(locus_dir, "annotations")
    dir.create(annot_dir, showWarnings = FALSE, recursive = TRUE)
    annot_file <- file.path(annot_dir, paste0(lib_name, suffix))
    return(annot_file)
}
