#' Import cell type-specific interactomes
#'
#' Brain cell-specific epigenomic data from Nott et al. (2019).
#' @keywords internal
#' @family NOTT2019
#' @source
#' \href{https://doi.org/10.1126/science.aay0793}{Nott et al. (2019)}
#'
#' @importFrom dplyr %>% mutate group_by summarise
#' @importFrom data.table data.table rbindlist
NOTT2019_get_interactome <- function(annot_sub,
                                     top.consensus.pos,
                                     marker_key,
                                     verbose = TRUE) {
    Coordinates <- Interaction <- chr <- Cell_type <- Element <- End <-
        top.consensus.dist <- NULL;

    messager("++ NOTT2019:: Getting interactome data.", v = verbose)
    interact.cols <- grep("*_interactions", colnames(annot_sub), value = TRUE)
    interact.DT <- lapply(interact.cols, function(column) {
        coords <- strsplit(annot_sub[, column][[1]], ",")
        coord.dt <- lapply(coords, function(coord, .column = column) {
            data.table::data.table(
                Interaction = .column,
                Cell_type = marker_key[
                    gsub("\\_.*", "", .column)
                ],
                Coordinates = coord
            )
        }) %>% data.table::rbindlist()
        return(coord.dt)
    }) %>% data.table::rbindlist()
    interact.DT <- subset(
        interact.DT,
        !is.na(Coordinates) & Coordinates != ""
    ) %>%
        tidyr::separate(
            col = Coordinates,
            into = c("chr", "Start", "End"), sep = ":|-"
        ) %>%
        tidyr::separate(
            col = Interaction, into = c("Marker", "Element", NA),
            sep = "_", remove = FALSE
        )
    interact.DT <- interact.DT %>%
        # Standardize CHR (NCBI format)
        dplyr::mutate(
            chr = gsub("chr", "", chr),
            Cell_type_interaction = paste(Cell_type, "-", Element)
        )
    interact.DT$Cell_type <- interact.DT$Cell_type %>% as.character()
    interact.DT$Start <- as.numeric(interact.DT$Start)
    interact.DT$End <- as.numeric(interact.DT$End)
    # Summarise distance from different celltype enhancer interactions
    summarise_top.consensus.dist <- interact.DT %>%
        dplyr::mutate(top.consensus.dist = End - top.consensus.pos) %>%
        dplyr::group_by(Cell_type) %>%
        dplyr::summarise(top.consensus.dist = mean(top.consensus.dist))
    return(interact.DT)
}
