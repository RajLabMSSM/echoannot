#' Get regulatory regions: Nott2019
#'
#' Get epigenomic peak ranges from Nott2019.
#' @param as_granges Return results a \link[GenomicRanges]{GRanges} object
#' instead of a \link[data.table]{data.table}.
#' @param nThread Number of threads to parallelize across. 
#' @param verbose Print messages. 
#' 
#' @export
#' @family NOTT2019
#' @source \href{https://doi.org/10.1126/science.aay0793}{Nott et al. (2019)}
#'
#' @importFrom parallel mclapply
#' @importFrom data.table rbindlist 
NOTT2019_get_regulatory_regions <- function(as_granges = FALSE,
                                            nThread = 1,
                                            verbose = TRUE) {
    Name <- Cell_type <- NULL;
    messager("++ NOTT2019:: Getting regulatory regions data.",v=verbose)
    NOTT2019_interactome <- get_NOTT2019_interactome()
    selected_sheets <- grep("promoters$|enhancers$",
        names(NOTT2019_interactome),
        value = TRUE
    )
    regions <- parallel::mclapply(selected_sheets, function(s) {
        messager("Importing", s, "...")
        dat <- NOTT2019_interactome[[s]]
        dat$Name <- tolower(s)
        return(dat)
    }, mc.cores = nThread) %>% data.table::rbindlist(fill = TRUE)

    cell_dict <- c(
        "astrocyte" = "astrocytes",
        "neuronal" = "neurons",
        "oligo" = "oligo",
        "oligodendrocytes" = "oligo",
        "microglia" = "microglia"
    )
    regions_sub <- regions %>%
        tidyr::separate(Name,
            into = c("Cell_type", "Element"),
            remove = FALSE
        ) %>%
        dplyr::mutate(
            middle = as.integer(end - abs(end - start) / 2),
            Cell_type = cell_dict[Cell_type]
        )
    if (as_granges) {
        regions_sub <- echodata::dt_to_granges(dat = regions_sub, 
                                               chrom_col = "chr", 
                                               start_col = "start",
                                               end_col = "end",
                                               style = "NCBI")
    }
    return(regions_sub)
}
