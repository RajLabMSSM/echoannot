annotation_file_name <- function(locus_dir,
                                 lib_name) {
    annot_dir <- file.path(locus_dir, "annotations")
    dir.create(annot_dir, showWarnings = FALSE, recursive = TRUE)
    annot_file <- file.path(annot_dir, paste0(lib_name, ".rds"))
    return(annot_file)
}
