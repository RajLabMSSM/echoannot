#' Run GoShifter enrichment pipeline
#'
#' Run the \href{https://github.com/immunogenomics/goshifter}{GoShifter}
#' locus-specific enrichment pipeline. This function orchestrates the full
#' workflow: preparing SNP maps and LD files, querying ROADMAP annotations,
#' running the GoShifter permutation test, and collecting results.
#'
#' @param locus_dir Path to the locus-level results directory.
#' @param dat A \code{data.table} or \code{data.frame} with columns
#'   \code{SNP}, \code{CHR}, \code{POS}, and \code{P}.
#' @param SNP_group Character string labelling this group of SNPs
#'   (default \code{""}).
#' @param goshifter_path Path to the directory containing
#'   \code{goshifter.py}. If \code{NULL}, the bundled copy shipped with
#'   \pkg{echolocatoR} is used (see \code{\link{GOSHIFTER_find_folder}}).
#' @param permutations Number of permutations for the enrichment test
#'   (default \code{1000}).
#' @param ROADMAP_search A keyword query passed to
#'   \code{\link{ROADMAP_query}} to filter ROADMAP annotations
#'   (e.g. \code{"monocyte"}).
#' @param chromatin_states Character vector of chromatin state abbreviations
#'   to test enrichment for (default \code{c("TssA")}).
#' @param R2_filter LD r-squared threshold for GoShifter
#'   (default \code{0.8}).
#' @param overlap_threshold Minimum number of overlapping SNPs required
#'   before running GoShifter on an annotation (default \code{1}).
#' @param force_new_goshifter If \code{TRUE}, re-run even if results
#'   already exist on disk (default \code{FALSE}).
#' @param remove_tmps Remove intermediate GoShifter output files after
#'   collecting results (default \code{TRUE}).
#' @param verbose Print messages (default \code{TRUE}).
#' @param save_results Write combined results to a tab-delimited file
#'   (default \code{TRUE}).
#'
#' @returns A \code{data.table} of GoShifter enrichment results across
#'   all requested chromatin states.
#'
#' @source
#' \href{https://github.com/immunogenomics/goshifter}{GoShifter GitHub}
#' \href{https://pubmed.ncbi.nlm.nih.gov/26140449/}{
#'   Trynka et al. (2015) \emph{Am J Hum Genet}}
#'
#' @family GOSHIFTER
#' @export
#' @importFrom data.table fread rbindlist fwrite
#' @examples
#' \dontrun{
#' locus_dir <- echodata::locus_dir
#' dat <- echodata::BST1
#' gs_out <- GOSHIFTER(locus_dir = locus_dir, dat = dat)
#' }
GOSHIFTER <- function(locus_dir,
                      dat,
                      SNP_group = "",
                      goshifter_path = NULL,
                      permutations = 1000,
                      ROADMAP_search = "",
                      chromatin_states = c("TssA"),
                      R2_filter = 0.8,
                      overlap_threshold = 1,
                      force_new_goshifter = FALSE,
                      remove_tmps = TRUE,
                      verbose = TRUE,
                      save_results = TRUE) {

    ## Resolve GoShifter install path
    goshifter_path <- GOSHIFTER_find_folder(goshifter = goshifter_path)
    ## Clean up any leftover .pyc files
    pyc_files <- file.path(
        goshifter_path,
        c("chromtree.pyc", "data.pyc", "docopt.pyc", "functions.pyc")
    )
    suppressWarnings(file.remove(pyc_files))

    ## Chromatin-state key
    chromState_key <- data.table::fread(
        system.file("extdata/ROADMAP",
                     "ROADMAP_chromatinState_HMM.tsv",
                     package = "echoannot")
    )

    ## Create GoShifter results directory
    GS_results_path <- file.path(locus_dir, "GoShifter")
    dir.create(GS_results_path, showWarnings = FALSE, recursive = TRUE)
    results_file <- file.path(
        GS_results_path,
        paste0("GOSHIFTER.",
               paste(chromatin_states, collapse = "-"),
               ".", SNP_group, ".txt")
    )

    ## Prepare SNP-map file
    GOSHIFTER_create_snpmap(
        dat = dat,
        GS_results = GS_results_path,
        verbose = verbose
    )

    ## Prepare LD files
    GOSHIFTER_create_LD(
        locus_dir = locus_dir,
        dat = dat,
        verbose = verbose
    )

    ## Query ROADMAP annotations
    grl_roadmap <- ROADMAP_query(
        query_dat = dat,
        keyword_query = ROADMAP_search,
        nThread = 1,
        verbose = verbose
    )

    ## Use cached results if available
    if (file.exists(results_file) && !force_new_goshifter) {
        GS_RESULTS <- data.table::fread(results_file)
    } else {
        ## Run GoShifter for each chromatin state
        GS_RESULTS <- lapply(chromatin_states, function(cs) {
            tryCatch({
                messager("+ GoShifter:: Running on chromatin state subset:",
                         cs, v = verbose)
                GOSHIFTER_run(
                    goshifter_path = goshifter_path,
                    locus_dir = locus_dir,
                    GRlist = grl_roadmap,
                    dat = dat,
                    chromatin_state = cs,
                    R2_filter = R2_filter,
                    permutations = permutations,
                    remove_tmps = remove_tmps,
                    verbose = verbose,
                    overlap_threshold = overlap_threshold
                )
            }, error = function(e) {
                messager("GoShifter:: Error for chromatin state ",
                         cs, ": ", conditionMessage(e), v = verbose)
                NULL
            })
        }) |> data.table::rbindlist(fill = TRUE)
    }

    ## Save results
    if (save_results) {
        data.table::fwrite(GS_RESULTS, results_file,
                           sep = "\t", quote = FALSE)
    }

    ## Clean up intermediate output files
    if (remove_tmps) {
        tmp_suffixes <- c(".nperm1000.enrich",
                          ".nperm1000.locusscore",
                          ".nperm1000.snpoverlap")
        suppressWarnings(
            file.remove(file.path(GS_results_path,
                                  paste0("GS_results", tmp_suffixes)))
        )
    }

    return(GS_RESULTS)
}
