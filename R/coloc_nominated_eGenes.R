#' Nominate target genes within each locus
#'
#' Across all GWAS-QTL colocalization tests across all studies,
#' take the eGene with the highest colocalziation probability (PP.H4)
#' and assign it as the most likely causal gene in that locus.
#'
#' eQTL queries and colocalization test done with \pkg{catalogueR}.
#'
#' @examples
#' \dontrun{
#' merged_DT <- echodata::get_Nalls2019_merged()
#' base_url <- "~/Desktop/Fine_Mapping/Data/GWAS/Nalls23andMe_2019"
#' coloc_results_path <- file.path(
#'     base_url, "_genome_wide/COLOC/coloc.eQTL_Catalogue_ALL.csv.gz"
#' )
#' gg_egene <- coloc_nominated_eGenes(coloc_results,
#'     merged_DT = merged_DT,
#'     fill_var = NULL
#' )
#'
#' # QTL
#' base_url <- "/sc/hydra/projects/ad-omics/microglia_omics/Fine_Mapping"
#' coloc_results_path <- file.path(
#'     base_url,
#'     "Kunkle_Microglia_all_regions/QTL_merged_coloc_results.snp.tsv.gz"
#' )
#' merged_DT <- data.table::fread(
#'     file.path(
#'         "/pd-omics/brian/Fine_Mapping/Data/QTL",
#'         "Microglia_all_regions",
#'         "multiGWAS.microgliaQTL_finemapping.csv.gz"
#'     )
#' )
#' gg_egene <- coloc_nominated_eGenes(coloc_results,
#'     merged_DT = merged_DT,
#'     fill_var = NULL
#' )
#' }
#' @keywords internal
#' @rawNamespace import(ggplot2, except = c(geom_rect, ggsave))
#' @importFrom dplyr %>% group_by top_n slice mutate desc arrange
#' @importFrom data.table fread data.table
coloc_nominated_eGenes <- function(coloc_results,
                                   merged_DT,
                                   label_yaxis = TRUE,
                                   y_lab = "Locus",
                                   x_lab = NULL,
                                   fill_var = "PP.H4",
                                   text_size = 2,
                                   PP_threshold = NULL,
                                   nThread = 1,
                                   show_plot = TRUE,
                                   verbose = TRUE) {
    PP.H4 <- eGene <- Locus.GWAS <- SNP.PP.H4 <- Locus <- dummy <- NULL

    # Check Corces gene annotations against eQTL/coloc eGenes
    messager("+ SUMMARISE:: Nominating genes by top colocalized eQTL eGenes",
        v = verbose
    )
    if (is.data.frame(coloc_results)) {
        dat <- coloc_results
    } else {
        dat <- data.table::fread(coloc_results, nThread = nThread)
    }

    # for(column %in% c("gene","snp","chr"))
    # if('gene' %in%)

    top_eGenes <- dat %>%
        subset(PP.H4 > if (is.null(PP_threshold)) 0 else PP_threshold) %>%
        # Remove RP11 and other spurious RP genes
        subset(!(startsWith(eGene, "RP") | eGene == "NA" | is.na(eGene))) %>%
        dplyr::group_by(Locus.GWAS) %>%
        dplyr::top_n(n = 1, wt = PP.H4) %>%
        # Ensure only 1 eGene per Locus
        dplyr::arrange(
            dplyr::desc(PP.H4),
            dplyr::desc(SNP.PP.H4)
        ) %>%
        dplyr::group_by(Locus.GWAS) %>%
        dplyr::slice(1) %>%
        dplyr::mutate(Locus = as.character(Locus.GWAS))

    top_eGenes <- order_loci(
        dat = top_eGenes,
        merged_DT = merged_DT
    )
    top_eGenes <- subset(top_eGenes, !is.na(Locus))
    top_eGenes$dummy <- "Top\ncolocalized\neGene"

    if (is.null(fill_var)) {
        text_color <- "grey20"
    } else {
        text_color <- "white"
    }

    gg_egene <- ggplot(top_eGenes, aes(x = dummy, y = Locus)) +
        labs(x = x_lab, y = y_lab) +
        geom_tile(fill = "transparent") +
        geom_text(aes(label = eGene), color = text_color, size = text_size) +
        # scale_fill_viridis_c(end = .8, na.value = "transparent") +
        # scale_fill_gradient(low = "blue", high = "red",
        # na.value = "transparent") +
        theme_bw() +
        # scale_x_discrete(position = "top") +
        theme( # axis.text.x = element_blank(),
            legend.box = "vertical",
            legend.position = "top",
            legend.text = element_text(size = 8),
            legend.text.align = .5,
            plot.margin = unit(rep(.1, 4), "cm")
        ) +
        guides(
            colour = guide_colourbar(title.position = "top", title.hjust = 0.5),
            size = guide_legend(title.position = "top", title.hjust = 0.5)
        ) +
        scale_y_discrete(drop = F)
    if (label_yaxis == F) {
        gg_egene <- gg_egene + theme(axis.text.y = element_blank())
    }
    if (show_plot) print(gg_egene)
    return(list(
        data = data.table::data.table(top_eGenes),
        plot = gg_egene
    ))
}
