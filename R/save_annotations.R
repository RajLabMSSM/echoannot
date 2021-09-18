save_annotations <- function(gr, 
                             anno_path=tempfile(fileext = ".rds"),
                             libName,
                             verbose=TRUE){
    messager("Saving annotations ==>", anno_path, v=verbose)
    dir.create(dirname(anno_path), showWarnings = FALSE, recursive = TRUE)
    saveRDS(gr, file.path(anno_path))
}