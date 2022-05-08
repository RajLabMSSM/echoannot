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
#' gg_egene <- coloc_nominated_egenes(coloc_results,
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
#' gg_egene <- coloc_nominated_egenes(coloc_results,
#'     merged_DT = merged_DT,
#'     fill_var = NULL
#' )
#' }
#' @keywords internal
#' @importFrom dplyr %>% group_by top_n slice mutate desc arrange
#' @importFrom data.table fread data.table
coloc_nominated_egenes <- function(coloc_results,
                                   merged_DT,
                                   label_yaxis = TRUE,
                                   y_lab = "Locus",
                                   x_lab = NULL,
                                   fill_var = "PP.H4",
                                   text_size = 2,
                                   credset_thresh = NULL,
                                   nThread = 1,
                                   show_plot = TRUE,
                                   verbose = TRUE) {
    
    requireNamespace("ggplot2")
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
        subset(PP.H4 > if (is.null(credset_thresh)) 0 else credset_thresh) %>%
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

    gg_egene <- ggplot2::ggplot(top_eGenes, 
                                ggplot2::aes(x = dummy, y = Locus)) +
        ggplot2::labs(x = x_lab, y = y_lab) +
        ggplot2::geom_tile(fill = "transparent") +
        ggplot2::geom_text(ggplot2::aes(label = eGene), 
                           color = text_color, size = text_size) +
        ggplot2::theme_bw() +
        ggplot2::theme(
            legend.box = "vertical",
            legend.position = "top",
            legend.text = ggplot2::element_text(size = 8),
            legend.text.align = .5,
            plot.margin = ggplot2::unit(rep(.1, 4), "cm")
        ) +
        ggplot2::guides(
            colour = ggplot2::guide_colourbar(title.position = "top",
                                              title.hjust = 0.5),
            size = ggplot2::guide_legend(title.position = "top",
                                         title.hjust = 0.5)
        ) +
        ggplot2::scale_y_discrete(drop = FALSE)
    if (label_yaxis == FALSE) {
        gg_egene <- gg_egene + 
            ggplot2::theme(axis.text.y = ggplot2::element_blank())
    }
    if (show_plot) print(gg_egene)
    return(list(
        data = data.table::data.table(top_eGenes),
        plot = gg_egene
    ))
}
