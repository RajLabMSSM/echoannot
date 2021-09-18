#' Adds fine-mapping summary columns
#'
#' Adds several columns that summarise the results across all fine-mapping
#' tools that were run:
#' \describe{
#' \item{Support}{The number of tools in which the SNP was proposed
#' in a credible set.}
#' \item{mean.PP}{The mean per-SNP PP across all fine-mapping tools used.}
#' \item{Consensus_SNP}{Whether or not the SNP was in the credible set of >
#'  \code{consensus_thresh} SNPs (\emph{default=2}).}
#' }
#' @family finemapping functions
#' @param consensus_thresh Threshold for determining
#' \strong{Consensus_SNP} status.
#' @keywords internal
#' @examples
#' merged_DT <- find_consensus_SNPs(finemap_dat = echodata::Nalls2019_merged)
#' @importFrom dplyr %>% desc arrange
find_consensus_SNPs <- function(finemap_dat,
                                credset_thresh = .95,
                                consensus_thresh = 2,
                                sort_by_support = TRUE,
                                exclude_methods = NULL,
                                top_CS_only = FALSE,
                                replace_PP_NAs = TRUE,
                                verbose = FALSE) {
    Consensus_SNP <- NULL

    messager("+ Identifying Consensus SNPs...", v = verbose)
    exclude_methods <- append(exclude_methods, "mean")
    # Find SNPs that are in the credible set for all fine-mapping tools
    finemap_dat <- update_cols(finemap_dat = finemap_dat)
    CS_cols <- grep(".CS$", colnames(finemap_dat), value = TRUE)
    CS_cols <- CS_cols[!(CS_cols %in%
        c(
            paste0(exclude_methods, ".CS"),
            paste0(exclude_methods, ".Credible_Set")
        ))]
    if (consensus_thresh == "all") {
        consensus_thresh <- length(CS_cols)
    }
    messager("++ support_thresh =", consensus_thresh, v = verbose)
    # Get the number of tools supporting each SNP
    ## Make sure each CS is set to 1
    support_sub <- subset(finemap_dat, select = CS_cols) %>% data.frame()
    if (top_CS_only) {
        messager("++ top_CS_only=TRUE", v = verbose)
        support_sub <- ifelse(support_sub == 1, 1, 0)
    } else {
        messager("++ top_CS_only=FALSE", v = verbose)
        support_sub <- ifelse(support_sub >= 1, 1, 0)
    }

    finemap_dat$Support <- rowSums(support_sub, na.rm = T)
    finemap_dat$Consensus_SNP <- finemap_dat$Support >= consensus_thresh
    # Sort
    if (sort_by_support) {
        finemap_dat <- finemap_dat %>%
            dplyr::arrange(
                dplyr::desc(Consensus_SNP),
                dplyr::desc(Support)
            )
    }
    # Calculate mean PP
    messager("+ Calculating mean Posterior Probability (mean.PP)...", v = verbose)
    PP.cols <- grep(".PP$", colnames(finemap_dat), value = TRUE)
    PP.cols <- PP.cols[!(PP.cols %in% paste0(exclude_methods, ".PP"))]
    PP.sub <- subset(finemap_dat, select = c("SNP", PP.cols)) %>% data.frame()
    if (replace_PP_NAs) {
        messager("+ Replacing PP==NA with 0", v = verbose)
        PP.sub[is.na(PP.sub)] <- 0
    }
    if (NCOL(PP.sub[, -1]) > 1) {
        finemap_dat$mean.PP <- rowMeans(PP.sub[, -1], na.rm = TRUE)
    } else {
        finemap_dat$mean.PP <- PP.sub[, -1]
    }
    finemap_dat$mean.CS <- ifelse(finemap_dat$mean.PP >= credset_thresh, 1, 0)

    # PP.sub %>% arrange(desc(mean.PP)) %>% head()
    messager("++", length(CS_cols),
        "fine-mapping methods used.",
        v = verbose
    )
    messager("++", dim(subset(finemap_dat, Support > 0))[1],
        "Credible Set SNPs identified.",
        v = verbose
    )
    messager("++", dim(subset(finemap_dat, Consensus_SNP == T))[1],
        "Consensus SNPs identified.",
        v = verbose
    )
    return(finemap_dat)
}
