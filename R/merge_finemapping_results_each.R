#' Create full cross-locus merged files for each dataset,
#' then return a subset of those files as one super-merged table.
#'
#'
#' @keywords internal
#' @importFrom data.table fread rbindlist fwrite
merge_finemapping_results_each <- function(study_dirs,
                                           LD_reference = "1KGphase3",
                                           minimum_support = 1,
                                           include_leadSNPs = TRUE,
                                           return_filter = "!is.na(SNP)",
                                           merged_path = "merged_DT.csv.gz",
                                           force_new_merge = FALSE,
                                           nThread = 1,
                                           verbose = TRUE) {
    Support <- Gene <- NULL;

    if (file.exists(merged_path) & force_new_merge == FALSE) {
        merged_DT <- data.table::fread(merged_path, nThread = nThread)
    } else {
        merged_DT <- lapply(
            study_dirs,
            function(study_dir) {
                messager("Study:", basename(study_dir))
                merged_all <- merge_finemapping_results(
                    dataset = study_dir,
                    LD_reference = LD_reference,
                    minimum_support = minimum_support,
                    include_leadSNPs = include_leadSNPs,
                    save_path = file.path(study_dir, paste(basename(study_dir), LD_reference, "merged.csv.gz", sep = ".")),
                    verbose = verbose
                )
                # Return subset for merged file
                merged_top <- subset(merged_all, eval(parse(text = return_filter)))
                return(merged_top)
            }
        ) |> data.table::rbindlist(fill = TRUE)
        # Save merged multi-study file
        if (merged_path != FALSE) {
            messager("+ SUMMARISE:: Saving merged subset after filtering criterion:", return_filter, v = verbose)
            data.table::fwrite(merged_DT, merged_path,
                nThread = nThread, sep = ","
            )
        }
    }
    return(merged_DT)
}
