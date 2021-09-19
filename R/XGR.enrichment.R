#' XGR enrichment
#'
#' Run SNP-level enrichment test with \link[XGR]{xGRviaGenomicAnno}.
#'
#' @param gr Annotations to test for enrichment with.
#' @param merged_dat SNP-level fine-mapping results to test for enrichment with.
#' @param foreground_filter Filter to apply to foreground (target SNPs).
#' @param background_filter Filter to apply to background (non-target SNPs).
#' @param grouping_vars Columns in \code{merged_dat} to group by when conducting
#' enrichment tests.
#' @param fg_sample_size Foreground sample size.
#' @param bg_sample_size Background sample size.
#' @param background.annotatable.only For background SNPs,
#' only use SNPs that overlap with some annotation in \code{gr}.
#'  This means that missing annotations (\code{NA}) will not be considered.
#' @param verbose Print messages.
#'
#' @family XGR
#' @examples
#' \dontrun{
#' gr.merged <- echoannot::merge_celltype_specific_epigenomics()
#' enrich.lead <- XGR.enrichment(
#'     gr = gr.merged,
#'     merged_dat = echodata::get_Nalls2019_merged(),
#'     foreground_filter = "leadSNP==TRUE",
#'     grouping_vars = c("Study", "Cell_type", "Assay")
#' )
#' }
#' @export
#' @importFrom data.table rbindlist
#' @importFrom XGR xGRviaGenomicAnno
XGR.enrichment <- function(gr,
                           merged_dat,
                           foreground_filter = "Consensus_SNP==TRUE",
                           background_filter = NULL,
                           grouping_vars = c(
                               "Study",
                               "Assay",
                               "Cell_type"
                           ),
                           fg_sample_size = NULL,
                           bg_sample_size = NULL,
                           background.annotatable.only = FALSE,
                           verbose = TRUE) {
    fg_bg <- XGR.prepare_foreground_background(
        subset_DT = merged_dat,
        foreground_filter = foreground_filter,
        background_filter = background_filter,
        fg_sample_size = fg_sample_size,
        bg_sample_size = bg_sample_size
    )
    # Create all combinations
    # if(!is.null(grouping_vars)){
    #   combos <- expand.grid(sapply( subset(data.frame(gr),
    # select=grouping_vars), unique)) %>%
    #     `colnames<-`(grouping_vars)
    #   if(length(grouping_vars)<2) {combos$dummy1 <- 1; gr$dummy1 <- 1; }
    # }else {
    #   combos <- data.frame(dummy1=1, dummy2=2);
    #   gr$dummy1 <- 1;  gr$dummy2 <- 2;
    # }

    combos <- unique(data.frame(gr)[, grouping_vars])
    combos[is.na(combos)] <- "NA"

    messager("+ XGR:: Conducting enrichment tests for",
        nrow(combos), "combinations of `grouping_vars`.",
        v = verbose
    )
    RES <- lapply(
        seq(1, nrow(combos)),
        function(i,
                 .background.annotatable.only =
                     background.annotatable.only) {
            ROW <- combos[i, ]
            # messager("+ XGR::",ROW)
            gr.sub <- gr
            for (column in colnames(combos)) {
                gr.sub <- subset(
                    gr.sub,
                    eval(parse(text = column)) == ROW[[column]]
                )
            }

            res <- suppressMessages(
                XGR::xGRviaGenomicAnno(
                    data.file = fg_bg$foreground,
                    background.file = fg_bg$background,
                    format.file = "data.frame",
                    GR.annotation = gr.sub,
                    background.annotatable.only =
                        .background.annotatable.only,
                    verbose = FALSE
                )
            )
            for (column in colnames(combos)) {
                res[[column]] <- ROW[[column]]
            }
            return(res)
        }
    ) %>% data.table::rbindlist()
    RES$fg_filter <- if (is.null(foreground_filter)) {
        NA
    } else {
        foreground_filter
    }
    RES$bg_filter <- if (is.null(background_filter)) {
        NA
    } else {
        background_filter
    }
    RES$fg_sample_size <- if (is.null(fg_sample_size)) {
        nrow(fg_bg$foreground)
    } else {
        fg_sample_size
    }
    RES$bg_sample_size <- if (is.null(bg_sample_size)) {
        nrow(merged_dat)
    } else {
        bg_sample_size
    }
    return(RES)
}
