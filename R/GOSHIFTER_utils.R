#' Find GoShifter installation folder
#'
#' Locate the directory containing the \code{goshifter.py} script.
#' By default, looks for the bundled copy shipped with
#' \pkg{echolocatoR} at \code{inst/tools/goshifter/}. A user-supplied
#' path takes precedence.
#'
#' @param goshifter Path to the GoShifter directory. If \code{NULL},
#'   the bundled copy in \pkg{echolocatoR} is used.
#'
#' @returns Path to the GoShifter directory.
#'
#' @family GOSHIFTER
#' @keywords internal
GOSHIFTER_find_folder <- function(goshifter = NULL) {

    if (is.null(goshifter)) {
        goshifter <- system.file("tools/goshifter",
                                 package = "echolocatoR")
        if (!nzchar(goshifter)) {
            stop("GoShifter directory not found. ",
                 "Provide the path via the 'goshifter' argument, ",
                 "or ensure echolocatoR is installed.",
                 call. = FALSE)
        }
    }
    if (!dir.exists(goshifter)) {
        stop("GoShifter directory does not exist: ", goshifter,
             call. = FALSE)
    }
    return(goshifter)
}


#' Search ROADMAP annotation reference for GoShifter
#'
#' Search the ROADMAP Epigenomic reference file for annotation BED
#' files matching filter criteria or a fuzzy keyword search.
#'
#' @param Roadmap_reference Path to the ROADMAP reference file
#'   (default: bundled file in \pkg{echoannot}).
#' @param EID_filter Filter by Epigenome ID(s).
#' @param GROUP_filter Filter by GROUP column.
#' @param ANATOMY_filter Filter by ANATOMY column.
#' @param GR_filter Filter by GR column.
#' @param fuzzy_search Keyword(s) to search across all annotation
#'   columns (case-insensitive).
#' @param verbose Print messages (default \code{TRUE}).
#'
#' @returns A \code{data.table} of matching ROADMAP annotation entries.
#'
#' @family GOSHIFTER
#' @keywords internal
#' @importFrom data.table fread
GOSHIFTER_search_ROADMAP <- function(Roadmap_reference =
                                         system.file(
                                             "extdata/ROADMAP",
                                             "ROADMAP_Epigenomic.js",
                                             package = "echoannot"
                                         ),
                                     EID_filter = NA,
                                     GROUP_filter = NA,
                                     ANATOMY_filter = NA,
                                     GR_filter = NA,
                                     fuzzy_search = NA,
                                     verbose = TRUE) {

    messager("++ GoShifter:: Searching for Roadmap annotation BED files...",
             v = verbose)
    fuzzy_search_str <- paste(fuzzy_search, collapse = "|")
    RM_ref <- suppressWarnings(
        data.table::fread(Roadmap_reference, skip = 1, header = FALSE,
                          col.names = c("EID", "GROUP", "ANATOMY", "GR",
                                        "Epigenome.Mnemonic",
                                        "Standardized.Epigenome.name",
                                        "Epigenome.name", "TYPE"))
    )

    if (!all(is.na(EID_filter)))
        RM_ref <- subset(RM_ref, RM_ref$EID %in% EID_filter)
    if (!all(is.na(GROUP_filter)))
        RM_ref <- subset(RM_ref, RM_ref$GROUP %in% GROUP_filter)
    if (!all(is.na(ANATOMY_filter)))
        RM_ref <- subset(RM_ref, RM_ref$ANATOMY %in% ANATOMY_filter)
    if (!all(is.na(GR_filter)))
        RM_ref <- subset(RM_ref, RM_ref$GR %in% GR_filter)

    if (!all(is.na(fuzzy_search))) {
        pattern <- tolower(fuzzy_search_str)
        match_rows <- grepl(pattern, tolower(RM_ref$GROUP)) |
            grepl(pattern, tolower(RM_ref$ANATOMY)) |
            grepl(pattern, tolower(RM_ref$GR)) |
            grepl(pattern, tolower(RM_ref$Standardized.Epigenome.name)) |
            grepl(pattern, tolower(RM_ref$Epigenome.name)) |
            grepl(pattern, tolower(RM_ref$TYPE))
        RM_ref <- RM_ref[match_rows, ]
    }

    total_found <- nrow(RM_ref)
    if (total_found > 0) {
        messager("++ GoShifter:: Found", total_found,
                 "annotation BED files matching search query.",
                 v = verbose)
    } else {
        stop("No annotation BED files found matching the search query.",
             call. = FALSE)
    }
    return(RM_ref)
}


#' Generate ROADMAP BED file names
#'
#' Construct the file names for ROADMAP chromHMM BED files based on
#' Epigenome IDs in the reference table.
#'
#' @param RM_ref A \code{data.table} of ROADMAP reference entries
#'   (must include an \code{EID} column).
#' @param suffix File name suffix
#'   (default \code{"_15_coreMarks_mnemonics.bed.gz"}).
#'
#' @returns A character vector of BED file names.
#'
#' @family GOSHIFTER
#' @keywords internal
GOSHIFTER_bed_names <- function(RM_ref,
                                suffix = "_15_coreMarks_mnemonics.bed.gz") {

    ## Files hosted at:
    ## https://egg2.wustl.edu/roadmap/data/byFileType/
    ##   chromhmmSegmentations/ChmmModels/coreMarks/jointModel/final/
    bed_names <- paste0(unique(RM_ref$EID), suffix)
    return(bed_names)
}


#' List ROADMAP chromatin states
#'
#' Read the chromatin state key from the ROADMAP HMM reference file.
#'
#' @param annotations_path Directory containing the \code{ROADMAP/}
#'   subdirectory with the chromatin state file. Defaults to the
#'   bundled extdata in \pkg{echoannot}.
#'
#' @returns A \code{data.table} mapping chromatin state codes to
#'   descriptions.
#'
#' @family GOSHIFTER
#' @keywords internal
#' @importFrom data.table fread
GOSHIFTER_list_chromatin_states <- function(annotations_path = NULL) {

    if (is.null(annotations_path)) {
        path <- system.file("extdata/ROADMAP",
                            "ROADMAP_chromatinState_HMM.tsv",
                            package = "echoannot")
    } else {
        path <- file.path(annotations_path, "ROADMAP",
                          "ROADMAP_chromatinState_HMM.tsv")
    }
    chromState_key <- data.table::fread(path)
    return(chromState_key)
}


#' Download and subset ROADMAP annotations for GoShifter
#'
#' Download ROADMAP chromHMM BED files from the WashU Epigenome
#' Browser, optionally subset to a specific chromatin state, and
#' return paths to the processed files.
#'
#' @param annotations_path Directory where annotation files are
#'   stored/cached.
#' @param bed_list Character vector of BED file names (as produced by
#'   \code{\link{GOSHIFTER_bed_names}}).
#' @param chromatin_state Chromatin state to subset by
#'   (e.g. \code{"TssA"}). Set to \code{NA} to keep all states.
#' @param verbose Print messages (default \code{TRUE}).
#'
#' @returns A character vector of paths to processed BED files
#'   (gzipped).
#'
#' @family GOSHIFTER
#' @keywords internal
#' @importFrom data.table fread fwrite tstrsplit
#' @importFrom utils download.file
GOSHIFTER_get_roadmap_annotations <- function(annotations_path = "./annotations",
                                              bed_list,
                                              chromatin_state = "TssA",
                                              verbose = TRUE) {

    State <- NULL
    output_paths <- vapply(bed_list, function(bed) {
        eid <- strsplit(bed, "_")[[1]][1]
        roadmap_annot_path <- file.path(annotations_path,
                                        "ROADMAP", "Chromatin_Marks")
        dir.create(roadmap_annot_path,
                   showWarnings = FALSE, recursive = TRUE)
        output_path <- file.path(roadmap_annot_path, bed)
        bed_subset <- file.path(
            dirname(output_path),
            paste0(eid, "_", chromatin_state, "_subset.bed")
        )
        bed_subset_gz <- paste0(bed_subset, ".gz")
        bed_url <- file.path(
            "https://egg2.wustl.edu/roadmap/data/byFileType",
            "chromhmmSegmentations/ChmmModels/coreMarks",
            "jointModel/final",
            bed
        )

        if (file.exists(bed_subset_gz)) {
            ## Already have the exact subset
            messager("+++ GoShifter:: Importing previously downloaded ",
                     "BED subset: ", bed_subset_gz, v = verbose)

        } else {
            ## Need to download or read full BED, then subset
            if (file.exists(output_path)) {
                messager("+++ GoShifter:: Importing previously downloaded ",
                         "full BED: ", output_path, v = verbose)
                dat <- data.table::fread(
                    output_path,
                    col.names = c("Chrom", "Start", "End", "State")
                )
            } else {
                messager("+++ GoShifter:: Downloading annotation BED from ",
                         "Roadmap server...", v = verbose)
                dat <- data.table::fread(
                    bed_url,
                    col.names = c("Chrom", "Start", "End", "State")
                )
                data.table::fwrite(
                    dat,
                    file.path(roadmap_annot_path, bed),
                    col.names = FALSE, sep = " "
                )
            }

            ## Split "Num_State" into separate columns
            dat[, c("Num", "State") :=
                    data.table::tstrsplit(State, "_", fixed = TRUE)]

            if (!is.na(chromatin_state)) {
                messager("+++ GoShifter:: Subsetting BED by chromatin ",
                         "state: ", chromatin_state, v = verbose)
                dat <- dat[dat$State == chromatin_state, ]
                data.table::fwrite(
                    dat, bed_subset,
                    col.names = FALSE, sep = "\t"
                )
                R.utils::gzip(bed_subset,
                              destname = bed_subset_gz,
                              overwrite = TRUE)
            } else {
                messager("+++ GoShifter:: Including all ",
                         length(unique(dat$State)),
                         " chromatin states.", v = verbose)
                bed_subset_gz <- file.path(roadmap_annot_path, bed)
            }
            messager("+++ GoShifter:: BED file saved to: ",
                     bed_subset_gz, v = verbose)
        }
        return(bed_subset_gz)
    }, character(1))

    return(output_paths)
}
