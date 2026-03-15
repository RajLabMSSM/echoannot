#' Process GoShifter output files
#'
#' Read the \code{.locusscore} and \code{.enrich} output files written
#' by GoShifter and merge them with ROADMAP reference metadata.
#'
#' @param locus_dir Path to the locus-level results directory.
#' @param output_bed Path to the annotation BED file that was tested.
#' @param out_prefix Prefix used when writing GoShifter output files
#'   (typically the annotation name).
#'
#' @returns A \code{data.table} with per-locus scores, p-values, and
#'   ROADMAP metadata.
#'
#' @family GOSHIFTER
#' @keywords internal
#' @importFrom data.table fread data.table merge.data.table set
#' @importFrom dplyr summarise
#' @importFrom rlang .data
GOSHIFTER_process_results <- function(locus_dir,
                                      output_bed,
                                      out_prefix) {

    ## ---- .LOCUSSCORE ----
    ## The likelihood of a locus to overlap an annotation under the null.
    ## Smaller values = more likely the overlap is non-random.
    locusscore <- data.table::fread(
        file.path(locus_dir, "GoShifter",
                  paste0(out_prefix, ".nperm1000.locusscore")),
        sep = "\t"
    )
    locusscore$Annotation <- basename(output_bed)
    eid <- strsplit(basename(output_bed), "_")[[1]][1]
    locusscore$EID <- eid

    ## Merge with ROADMAP reference
    roadmap_ref <- ROADMAP_construct_reference(verbose = FALSE)
    ref_row <- subset(roadmap_ref, roadmap_ref$EID == eid)
    GS_stats <- data.table::merge.data.table(
        locusscore,
        data.table::data.table(ref_row),
        by = "EID",
        all.x = TRUE
    )

    ## ---- .ENRICH ----
    ## Observed (nperm=0) and permuted overlap values.
    enrich <- data.table::fread(
        file.path(locus_dir, "GoShifter",
                  paste0(out_prefix, ".nperm1000.enrich")),
        sep = "\t"
    )
    ## Manually compute p-value: proportion of permutations where
    ## enrichment >= observed overlap count
    GS_stats$pval <- sum(enrich$enrichment >= enrich$nSnpOverlap) /
        max(enrich$nperm)

    ## Summary statistics
    mean_enrich <- enrich |>
        dplyr::summarise(
            mean_nSnpOverlap = mean(.data$nSnpOverlap),
            allSnps = mean(.data$allSnps),
            mean_enrichment = mean(.data$enrichment)
        )
    GS_stats$mean_nSnpOverlap <- mean_enrich$mean_nSnpOverlap
    GS_stats$allSnps <- mean_enrich$allSnps
    GS_stats$mean_enrichment <- mean_enrich$mean_enrichment

    ## Replace "N/A" strings with true NA
    for (col in names(GS_stats)) {
        na_idx <- which(GS_stats[[col]] == "N/A")
        if (length(na_idx) > 0) {
            data.table::set(GS_stats, i = na_idx, j = col, value = NA)
        }
    }

    return(GS_stats)
}


#' Summarise GoShifter results
#'
#' Filter GoShifter results to significant enrichments and report
#' per-tissue counts.
#'
#' @param GS_results A \code{data.table} of GoShifter results
#'   (as returned by \code{\link{GOSHIFTER}} or
#'   \code{\link{GOSHIFTER_run}}).
#' @param roadmap_ref A ROADMAP reference \code{data.table}
#'   (from \code{\link{ROADMAP_construct_reference}}).
#'   If \code{NULL}, it is constructed automatically.
#' @param chromatin_state Character vector of chromatin states tested.
#' @param verbose Print messages (default \code{TRUE}).
#'
#' @returns A \code{data.table} of significant results
#'   (enrichment p-value <= 0.05), arranged by enrichment.
#'
#' @family GOSHIFTER
#' @keywords internal
#' @importFrom dplyr arrange desc mutate group_by slice count
#' @importFrom rlang .data
GOSHIFTER_summarise_results <- function(GS_results,
                                        roadmap_ref = NULL,
                                        chromatin_state = NULL,
                                        verbose = TRUE) {

    if (is.null(roadmap_ref)) {
        roadmap_ref <- ROADMAP_construct_reference(verbose = FALSE)
    }

    sig_results <- GS_results |>
        subset(GS_results$enrichment <= 0.05) |>
        dplyr::arrange(dplyr::desc(.data$enrichment),
                       dplyr::desc(.data$nSnpOverlap)) |>
        dplyr::mutate(
            enrichment = formatC(.data$enrichment,
                                 format = "e", digits = 7)
        )

    ## Per-tissue/chromatin-state counts
    sig_count <- sig_results |>
        dplyr::group_by(.data$EID) |>
        dplyr::slice(1) |>
        dplyr::group_by(.data$chromatin_state) |>
        dplyr::count()

    if (!is.null(chromatin_state) && nrow(sig_count) > 0) {
        messager("+++", sig_count$n, "/", nrow(roadmap_ref),
                 "tissues had significant enrichment for:",
                 paste(chromatin_state, collapse = ", "),
                 v = verbose)
    }

    return(sig_results)
}


#' Check SNP-annotation overlap for GoShifter
#'
#' Determine which annotation BED regions overlap with the SNPs in
#' the GoShifter snpmap file.
#'
#' @param output_bed Path to the annotation BED file.
#' @param GS_results_path Path to the GoShifter results directory
#'   (must contain \code{snpmap.txt}).
#' @param overlap_threshold Minimum overlaps required (not actively
#'   filtered here; used for reference only).
#'
#' @returns A \code{data.table} of BED regions overlapping with the SNPs.
#'
#' @family GOSHIFTER
#' @keywords internal
#' @importFrom data.table fread rbindlist
GOSHIFTER_check_overlap <- function(output_bed,
                                    GS_results_path,
                                    overlap_threshold = 1) {

    bed_file <- data.table::fread(
        output_bed,
        col.names = c("Chrom", "Start", "End", "State", "Extra")
    )
    snpmap <- data.table::fread(
        file.path(GS_results_path, "snpmap.txt")
    )

    ## Find overlapping BED regions
    bed_overlap <- lapply(seq_len(nrow(snpmap)), function(i) {
        row <- snpmap[i, ]
        subset(bed_file,
               bed_file$Chrom == row$Chrom &
               bed_file$Start <= row$BP &
               bed_file$End >= row$BP)
    }) |> data.table::rbindlist(fill = TRUE)

    return(bed_overlap)
}
