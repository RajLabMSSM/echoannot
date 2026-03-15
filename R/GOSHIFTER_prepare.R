#' Create GoShifter SNP-map file
#'
#' Prepare the tab-delimited SNP-map input file required by GoShifter.
#' The file contains columns \code{SNP}, \code{Chrom} (prefixed with
#' \code{"chr"}), and \code{BP}.
#'
#' @param dat A \code{data.table} or \code{data.frame} with columns
#'   \code{SNP}, \code{CHR}, and \code{POS}.
#' @param GS_results Path to the GoShifter results directory where the
#'   \code{snpmap.txt} file will be written.
#' @param verbose Print messages (default \code{TRUE}).
#'
#' @returns Path to the written \code{snpmap.txt} file (invisibly).
#'
#' @family GOSHIFTER
#' @keywords internal
#' @importFrom dplyr rename mutate select
#' @importFrom data.table fwrite
#' @importFrom rlang .data
#' @examples
#' \dontrun{
#' dat <- echodata::BST1
#' locus_dir <- echodata::locus_dir
#' snpmap <- GOSHIFTER_create_snpmap(
#'     dat = dat,
#'     GS_results = file.path(locus_dir, "GoShifter")
#' )
#' }
GOSHIFTER_create_snpmap <- function(dat,
                                    GS_results,
                                    verbose = TRUE) {

    messager("+ GOSHIFTER:: Creating snpmap file...", v = verbose)
    snpmap <- file.path(GS_results, "snpmap.txt")
    dir.create(GS_results, showWarnings = FALSE, recursive = TRUE)

    snpmap_dat <- dat |>
        dplyr::rename(Chrom = "CHR", BP = "POS") |>
        dplyr::mutate(Chrom = paste0("chr", .data$Chrom)) |>
        dplyr::select("SNP", "Chrom", "BP")

    data.table::fwrite(snpmap_dat, snpmap, sep = "\t")
    messager("++ GOSHIFTER:: snpmap written to:", snpmap, v = verbose)
    return(invisible(snpmap))
}


#' Create GoShifter LD files
#'
#' Prepare LD input files for GoShifter. Accepts either a sparse LD
#' matrix RDS file (from \pkg{echoLD}) or a PLINK \code{.ld} file.
#' The output is a set of per-chromosome, bgzipped and tabix-indexed
#' LD files.
#'
#' @param locus_dir Path to the locus-level results directory.
#' @param dat A \code{data.table} or \code{data.frame} with at least
#'   columns \code{SNP}, \code{CHR}, and \code{POS}. If \code{NULL},
#'   will attempt to read from the multi-finemap results in
#'   \code{locus_dir}.
#' @param LD_path Path to the LD file. If \code{NULL}, the first
#'   \code{.RDS} file in \code{<locus_dir>/LD/} is used.
#' @param conda_env Conda environment name in which \code{bgzip} and
#'   \code{tabix} can be found (default \code{"goshifter"}).
#' @param nThread Number of threads for file I/O (default \code{1}).
#' @param verbose Print messages (default \code{TRUE}).
#'
#' @returns Path to the LD output folder (invisibly).
#'
#' @family GOSHIFTER
#' @keywords internal
#' @importFrom data.table fread data.table merge.data.table fwrite
#' @importFrom dplyr rename mutate arrange select
#' @importFrom rlang .data
#' @examples
#' \dontrun{
#' dat <- echodata::BST1
#' locus_dir <- echodata::locus_dir
#' LD_folder <- GOSHIFTER_create_LD(
#'     locus_dir = locus_dir,
#'     dat = subset(dat, P < 5e-8)
#' )
#' }
GOSHIFTER_create_LD <- function(locus_dir,
                                dat = NULL,
                                LD_path = NULL,
                                conda_env = "goshifter",
                                nThread = 1,
                                verbose = TRUE) {

    rsIdA <- rsIdB <- NULL
    messager("+ GOSHIFTER:: Creating LD file(s)...", v = verbose)
    GS_results_path <- file.path(locus_dir, "GoShifter")

    ## Locate LD file
    if (is.null(LD_path)) {
        LD_path <- list.files(file.path(locus_dir, "LD"),
                              "\\.RDS$", full.names = TRUE)[1]
    }
    is_rds <- any(endsWith(LD_path, "_LD.RDS"), na.rm = TRUE)
    is_plink <- any(endsWith(LD_path, "plink.ld"), na.rm = TRUE)
    if (!(is_rds || is_plink)) {
        stop("No supported LD file detected. ",
             "Expected *_LD.RDS or *plink.ld.", call. = FALSE)
    }

    ## --- Sparse RDS from echoLD ---
    if (is_rds) {
        messager("+ GOSHIFTER:: Reformatting LD from RDS", v = verbose)
        if (!requireNamespace("echoLD", quietly = TRUE)) {
            stop("Package 'echoLD' is required to read sparse LD matrices. ",
                 "Install it with: ",
                 "remotes::install_github('RajLabMSSM/echoLD')",
                 call. = FALSE)
        }
        LD_matrix <- echoLD::readSparse(LD_path, convert_to_df = FALSE)
        LD_df <- as.data.frame(
            as.table(as.matrix(LD_matrix))
        )
        colnames(LD_df) <- c("rsIdA", "rsIdB", "Rsquared")
        LD_df$Rsquared <- LD_df$Rsquared^2

        ## Read fine-mapping results if dat not provided
        if (is.null(dat)) {
            fm_file <- list.files(
                file.path(locus_dir, "Multi-finemap"),
                "_Multi-finemap\\.tsv\\.gz$",
                full.names = TRUE
            )[1]
            dat <- data.table::fread(fm_file)
        }
        LD_df <- subset(LD_df, rsIdA %in% dat$SNP | rsIdB %in% dat$SNP)

        ## Merge positions from dat
        pos_a <- subset(dat, select = c("SNP", "CHR", "POS")) |>
            dplyr::rename(chrA = "CHR", posA = "POS")
        pos_b <- subset(dat, select = c("SNP", "CHR", "POS")) |>
            dplyr::rename(chrB = "CHR", posB = "POS")

        ld_file <- data.table::merge.data.table(
            data.table::data.table(LD_df), pos_a,
            by.x = "rsIdA", by.y = "SNP"
        )
        ld_file <- data.table::merge.data.table(
            ld_file, pos_b,
            by.x = "rsIdB", by.y = "SNP"
        )
        ld_file <- ld_file |>
            dplyr::mutate(
                chrA = paste0("chr", gsub("chr", "", .data$chrA)),
                chrB = paste0("chr", gsub("chr", "", .data$chrB))
            ) |>
            dplyr::arrange(.data$chrA, .data$posA) |>
            dplyr::select("chrA", "posA", "rsIdA",
                          "posB", "rsIdB", "Rsquared")
    }

    ## --- PLINK LD ---
    if (is_plink) {
        messager("++ GoShifter:: Reformatting 1000 Genomes LD", v = verbose)
        ld_file <- data.table::fread(LD_path) |>
            dplyr::rename(
                chrA = "CHR_A", posA = "BP_A", rsIdA = "SNP_A",
                posB = "BP_B", rsIdB = "SNP_B",
                Rsquared = "R", DPrime = "DP"
            ) |>
            dplyr::mutate(
                Rsquared = .data$Rsquared^2,
                chrA = paste0("chr", .data$chrA)
            ) |>
            dplyr::select("chrA", "posA", "rsIdA",
                          "posB", "rsIdB", "Rsquared", "DPrime")
    }

    ## Write per-chromosome bgzipped + tabix-indexed files
    if (!requireNamespace("echoconda", quietly = TRUE)) {
        stop("Package 'echoconda' is required for bgzip/tabix. ",
             "Install it with: ",
             "remotes::install_github('RajLabMSSM/echoconda')",
             call. = FALSE)
    }
    LD_folder <- file.path(GS_results_path, "LD")
    dir.create(LD_folder, showWarnings = FALSE, recursive = TRUE)

    bgzip_bin <- echoconda::find_packages(package = "bgzip",
                                          conda_env = conda_env)
    tabix_bin <- echoconda::find_packages(package = "tabix",
                                          conda_env = conda_env)

    for (chr in unique(ld_file$chrA)) {
        ld_chr_path <- file.path(LD_folder, paste0(chr, ".EUR.tsv"))
        gz_path <- paste0(ld_chr_path, ".gz")
        suppressWarnings(file.remove(gz_path))
        data.table::fwrite(
            subset(ld_file, ld_file$chrA == chr),
            ld_chr_path,
            sep = "\t", quote = FALSE,
            col.names = FALSE, nThread = nThread
        )
        system(paste(bgzip_bin, ld_chr_path))
        system(paste(tabix_bin, "-b 2 -e 2 -f", gz_path))
    }
    messager("+++ LD file(s) written to:", LD_folder, v = verbose)
    return(invisible(LD_folder))
}
