#' Plot inter-study SNP overlap
#'
#' Cross-tabulate SNP overlap (after applying filter)
#' between each pair of studies.
#' @param merged_DT Merged fine-mapping results data from
#'  \link[echolocatoR]{finemap_loci}.
#' @param triangle Plot correlation matrix as a square or a triangle.
#' @param filename Path to save file to as PNG.
#' @param formula_str Formula passed to \link[stats]{xtabs}.
#' @inheritParams super_summary_plot
#' @family summarise
#' 
#' @export 
#' @importFrom stats as.formula median 
#' @importFrom DescTools Divisors 
#' @importFrom grDevices png dev.off
#' @examples
#' \dontrun{
#'     merged_DT <- echodata::get_Nalls2019_merged()
#'     merged_DT$Dataset <- rep(c("Dataset1","Dataset2"),nrow(merged_DT)/2)
#'     snp_xprod <- plot_dataset_overlap(merged_DT = merged_DT)
#'     
#' }
plot_dataset_overlap <- function(merged_DT,
                                 snp_filter = "!is.na(SNP)",
                                 filename = NA,
                                 formula_str = "~ SNP + Dataset",
                                 triangle = FALSE) {
    
    snp_xtab <- subset(merged_DT, eval(parse(text = snp_filter)),
        .drop = FALSE
    ) |>
        BiocGenerics::xtabs(
            formula = stats::as.formula(formula_str),
            sparse = FALSE,
            drop.unused.levels = FALSE
        )
    snp_xprod <- crossprod(snp_xtab)
    diag(snp_xprod) <- NA
    mode(snp_xprod) <- "integer"

    if (triangle) {
        requireNamespace("corrplot")
        max_count <- max(snp_xprod, na.rm = TRUE)
        messager("max_count =", formatC(max_count,big.mark = ","))
        cl.length <- if (max_count <= 10) {
            max_count
        } else {
            stats::median(DescTools::Divisors(max_count)[[1]]) + 1
        }
        messager("cl.length =", cl.length)
        grDevices::png(filename, height = 500, width = 500, type = "cairo")
        dat <- corrplot::corrplot(
            corr = snp_xprod,
            method = "color",
            type = "lower",
            addgrid.col = "grey",
            tl.col = "black",
            hclust.method = "ward.D2",
            title = paste(
                "SNP overlap:",
                gsub("[|]", "\nOR", snp_filter)
            ),
            order = "hclust",
            cl.length = cl.length,
            mar = c(0, 0, 4, 4),
            # tl.pos = "lt",
            diag = FALSE,
            is.corr = FALSE
        )
        grDevices::dev.off()
    } else {
        requireNamespace("pheatmap")
        pheatmap::pheatmap(
            mat = snp_xprod,
            display_numbers = TRUE,
            filename = filename,
            # number_color = "white",
            main = paste("SNP overlap:", snp_filter),
            angle_col = 45,
            cluster_cols = TRUE,
            cluster_rows = TRUE,
            drop_levels = FALSE,
            na_col = "white"
        )
    }
    return(snp_xprod)
}
