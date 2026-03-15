#' Run GoShifter enrichment test
#'
#' Execute the
#' \href{https://github.com/immunogenomics/goshifter}{GoShifter}
#' Python tool on a set of SNPs against one or more annotation BED files.
#' Iterates over each element of \code{GRlist}, checks for overlap with
#' the SNP data, prepares annotation BED files, and calls the
#' \code{goshifter.py} script.
#'
#' @param dat A \code{data.table} or \code{data.frame} with at least
#'   columns \code{SNP}, \code{CHR}, \code{POS}, and \code{P}.
#' @param locus_dir Path to the locus-level results directory.
#' @param GRlist A named \code{GRangesList} of annotation regions to
#'   test for enrichment (e.g. from \code{\link{ROADMAP_query}}).
#' @param permutations Number of permutations (default \code{1000}).
#' @param goshifter_path Path to the directory containing
#'   \code{goshifter.py}. If \code{NULL}, uses the bundled copy
#'   (see \code{\link{GOSHIFTER_find_folder}}).
#' @param chromatin_state Chromatin state label to record in the results
#'   (default \code{"TssA"}).
#' @param R2_filter LD r-squared threshold (default \code{0.8}).
#' @param overlap_threshold Minimum number of overlapping SNPs required
#'   to run GoShifter for a given annotation (default \code{1}).
#' @param remove_tmps Remove intermediate BED files after processing
#'   (default \code{TRUE}).
#' @param verbose Print messages (default \code{TRUE}).
#'
#' @returns A \code{data.table} with GoShifter results for all tested
#'   annotations.
#'
#' @source
#' \href{https://github.com/immunogenomics/goshifter}{GoShifter GitHub}
#' \href{https://pubmed.ncbi.nlm.nih.gov/26140449/}{
#'   Trynka et al. (2015) \emph{Am J Hum Genet}}
#'
#' @family GOSHIFTER
#' @export
#' @importFrom data.table data.table rbindlist
#' @importFrom echodata granges_to_bed
#' @examples
#' \dontrun{
#' dat <- echodata::BST1
#' locus_dir <- echodata::locus_dir
#' peaks <- echoannot::NOTT2019_get_epigenomic_peaks()
#' grl_peaks <- GenomicRanges::makeGRangesListFromDataFrame(
#'     peaks, split.field = "Cell_type"
#' )
#' GS_results <- GOSHIFTER_run(
#'     dat = subset(dat, P < 5e-8),
#'     locus_dir = locus_dir,
#'     GRlist = grl_peaks
#' )
#' }
GOSHIFTER_run <- function(dat,
                          locus_dir,
                          GRlist,
                          permutations = 1000,
                          goshifter_path = NULL,
                          chromatin_state = "TssA",
                          R2_filter = 0.8,
                          overlap_threshold = 1,
                          remove_tmps = TRUE,
                          verbose = TRUE) {

    goshifter_path <- GOSHIFTER_find_folder(goshifter = goshifter_path)

    ## Require echoconda for python path resolution
    if (!requireNamespace("echoconda", quietly = TRUE)) {
        stop("Package 'echoconda' is required to run GoShifter. ",
             "Install it with: remotes::install_github('RajLabMSSM/echoconda')",
             call. = FALSE)
    }

    GS_results_path <- file.path(locus_dir, "GoShifter")
    dir.create(GS_results_path, showWarnings = FALSE, recursive = TRUE)

    ## Iterate over each annotation
    GS_results <- lapply(names(GRlist), function(gr_name) {

        messager("GOSHIFTER_run:: Processing annotation: ", gr_name,
                 v = verbose)
        gr <- GRlist[[gr_name]]

        ## Check for overlap between SNPs and annotation
        gr_hits <- granges_overlap(
            dat1 = dat,
            chrom_col.1 = "CHR",
            start_col.1 = "POS",
            end_col.1 = "POS",
            dat2 = gr,
            verbose = verbose
        )

        if (length(gr_hits) >= overlap_threshold) {
            messager("++ GoShifter:: Converting GRanges to BED",
                     v = verbose)
            ## Prepare annotation BED
            bed_dir <- file.path(locus_dir, "annotations", gr_name)
            bed_file <- echodata::granges_to_bed(
                GR.annotations = GRlist[gr_name],
                output_path = bed_dir,
                gzip = TRUE,
                nThread = 1
            )

            ## Prepare LD
            LD_folder <- GOSHIFTER_create_LD(
                locus_dir = locus_dir,
                dat = dat,
                verbose = verbose
            )

            ## Find python
            messager("GoShifter:: Overlapping SNPs detected", v = verbose)
            python <- echoconda::find_python_path(conda_env = "goshifter")
            if (python == "python") python <- "python2.7"

            ## Build command
            out_prefix <- file.path(GS_results_path, gr_name)
            cmd <- paste(
                python,
                file.path(goshifter_path, "goshifter.py"),
                "--snpmap", file.path(GS_results_path, "snpmap.txt"),
                "--annotation", bed_file,
                "--permute", permutations,
                "--ld", file.path(GS_results_path, "LD"),
                "--out", out_prefix
            )
            cmd_out <- system(cmd, intern = TRUE)
            if (verbose) cat(paste(cmd_out, collapse = "\n"), "\n")

            ## Gather results from written output files
            GS_stats <- GOSHIFTER_process_results(
                locus_dir = locus_dir,
                output_bed = bed_file,
                out_prefix = gr_name
            )
            GS_stats$chromatin_state <- chromatin_state

            ## Extract p-value from console output
            pval_line <- cmd_out[startsWith(cmd_out, "p-value")]
            GS_pval <- as.numeric(gsub("p-value = ", "", pval_line))
            GS_stats$GS_pval <- GS_pval

            ## Check overlap count
            bed_overlap <- GOSHIFTER_check_overlap(
                output_bed = bed_file,
                GS_results_path = GS_results_path,
                overlap_threshold = overlap_threshold
            )
            GS_stats$true_overlap <- nrow(bed_overlap)

            if (remove_tmps) {
                suppressWarnings(file.remove(bed_file))
            }
        } else {
            messager("GoShifter:: No overlap between snpmap and BED files",
                     " (overlap_threshold = ", overlap_threshold, ").",
                     v = verbose)
            GS_stats <- data.table::data.table(
                Annotation = gr_name,
                pval = NA_real_,
                GS_pval = NA_real_,
                true_overlap = 0L
            )
        }
        return(GS_stats)
    }) |> data.table::rbindlist(fill = TRUE)

    return(GS_results)
}
