#' Plot brain cell-specific interactome data
#'
#' Plot brain cell-specific interactome data from
#'  \href{https://doi.org/10.1126/science.aay0793}{Nott et al. (2019)}.
#'
#' @family NOTT2019
#' @source
#' \href{https://doi.org/10.1126/science.aay0793}{Nott et al. (2019)}
#'
#' @param highlight_plac Whether to scale opacity of PLAC-seq interactions 
#' (arches) such that interactions with anchors containing Consensus SNPs 
#' will be colored darker (Default: \code{TRUE}).
#' If \code{FALSE}, will instead apply the same opacity level
#'  to all interactions. 
#' @param show_plot Print plot.
#' @param return_interaction_track Return only the interaction track 
#' (before completing the plot and showing it). 
#' @param zoom_window Zoom window.
#' @param index_SNP Index/lead SNP RSID.
#' @param color_dict Named list of colors for each regulatory element. 
#' @param show_regulatory_rects Show enhancers/promoters as rectangles. 
#' @param show_anchors Show PLAC-seq anchors. 
#' @param point_size Point size of each SNP in the GWAS/fine-mapping plots.
#' @param strip.text.y.angle Angle of the y-axis facet labels. 
#' @inheritParams NOTT2019_epigenomic_histograms
#' @inheritParams convert_plots
#' @inheritParams ggbio::ggsave
#' @inheritParams ggplot2::theme
#' @inheritParams ggbio::autoplot
#' @inheritParams import_ucsc_bigwigs
#' @inheritParams get_window_limits
#' 
#' @export
#' @importFrom IRanges IRanges
#' @importFrom echodata find_consensus_snps
#' @importFrom dplyr top_n
#' @importFrom GenomicRanges seqnames start end GRanges findOverlaps
#' @importFrom S4Vectors subjectHits
#' @importFrom methods show
#' @examples
#' trks_plus_lines <- echoannot::NOTT2019_plac_seq_plot(dat = echodata::BST1) 
NOTT2019_plac_seq_plot <- function(dat = NULL,
                                   locus_dir = NULL,
                                   title = NULL,
                                   show_plot = TRUE,
                                   save_plot = TRUE,
                                   return_interaction_track = FALSE,
                                   x_limits = NULL,
                                   zoom_window = NULL,
                                   index_SNP = NULL,
                                   genomic_units = "POS", # Changing this to "Mb" tends to break everything
                                   color_dict = c(
                                       "enhancers" = "springgreen2",
                                       "promoters" = "purple",
                                       "anchors" = "black"
                                   ), 
                                   highlight_plac = TRUE,
                                   show_regulatory_rects = TRUE,
                                   show_anchors = TRUE,
                                   strip.text.y.angle = 0,
                                   xtext = TRUE,
                                   save_annot = FALSE,
                                   point_size = 2,
                                   height = 7,
                                   width = 7,
                                   dpi = 300,
                                   return_as = "Tracks",
                                   nThread = 1,
                                   verbose = TRUE) {
    # dat=echoannot::BST1; show_plot=T; save_plot=T; title=NULL;
    # x_limits=NULL; zoom_window=NULL; return_consensus_overlap =T; nThread=1;
    # highlight_plac=F; point_size=2;  index_SNP=NULL;
    # color_dict=c("enhancers"="springgreen",
    #              "promoters"="purple","anchors"="black");
    # genomic_units="Mb"; verbose=T; save_annot=F; show_anchors=T;
    # strip.text.y.angle <- 0
    
    requireNamespace("ggplot2")
    requireNamespace("ggbio")
    leadSNP <- SNP <- Consensus_SNP <- mean.PP <- Effect <- Support <-
        Start <- End <- y <- Element <- POS <- P <- NULL;
    messager("NOTT2019:: Creating PLAC-seq interactome plot", v = verbose)
    dat <- add_mb(dat = dat)
    xvar <- genomic_units
    if (!"Consensus_SNP" %in% colnames(dat)) {
        dat <-
            echodata::find_consensus_snps(
                dat = dat,
                verbose = FALSE
            )
    }
    marker_key <- NOTT2019_marker_key()
    lead.pos <- get_lead_pos(dat = dat, 
                             xvar = xvar,
                             index_SNP = index_SNP)
    consensus.pos <- get_consensus_pos(dat = dat, 
                                       xvar = xvar)
    top.consensus.pos <- get_top_consensus_pos(dat = dat,
                                               xvar = xvar)
    if (is.null(x_limits)) {
        x_limits <- c(
            min(dat[[xvar]], na.rm = TRUE),
            max(dat[[xvar]], na.rm = TRUE)
        )
    }
    ##### Get zoom window x_limits ####
    if (!is.null(zoom_window)) {
        x_limits <- get_zoom_xlims(lead.pos = lead.pos,
                                   zoom_window = zoom_window,
                                   verbose = verbose)
    }
    #### Get promoter interactome data ####
    annot_sub <- NOTT2019_get_promoter_interactome_data(dat = dat)
    promoter_celltypes <- NOTT2019_get_promoter_celltypes(
        annot_sub = annot_sub,
        marker_key = marker_key,
        verbose = verbose
    )
    #### get PLAC-seq junctions ####
    interact.DT <- NOTT2019_get_interactome(
        annot_sub = annot_sub,
        top.consensus.pos = top.consensus.pos,
        marker_key = marker_key
    )
    #### get promoter/enhancers ####
    regions <- NOTT2019_get_regulatory_regions(
        nThread = nThread,
        as_granges = TRUE,
        verbose = verbose
    )
    #### regions needs to be in dat range #### 
    regions <- subset(
        regions,
        as.character(GenomicRanges::seqnames(regions)) ==
            gsub("chr", "", unique(dat$CHR)[1]) &
            GenomicRanges::start(regions) >=
                min(dat[["POS"]], na.rm = TRUE) &
            GenomicRanges::end(regions) <=
                max(dat[["POS"]], na.rm = TRUE)
    )
    #### Plot PLAC-seq anchors ####
    if (show_anchors) {
        interact.anchors <- NOTT2019_get_interactions(
            dat = dat,
            as_granges = TRUE,
            verbose = verbose
        )
        GenomicRanges::mcols(interact.anchors)["Element"] <- "anchors"
        regions <- c(regions, interact.anchors)
    }
    
    ##### JH - which PLAC-Seq junctions overlap (5kb) the consensus SNPs? ####
    ### Run this step before saving 
    if (highlight_plac) { 
        interact.DT <- prepare_highlight_plac_data(dat = dat,
                                                   interact.DT = interact.DT,
                                                   verbose = verbose)
    }
    #### save annotations ####
    if (save_annot) {
        annot_file1 <- annotation_file_name(
            locus_dir = locus_dir,
            lib_name = "NOTT2019_interactome"
        )
        messager("Saving annotations ==>",annot_file1,v=verbose)
        saveRDS(interact.DT, annot_file1)
        annot_file2 <- annotation_file_name(
            locus_dir = locus_dir,
            lib_name = "NOTT2019_enhancers_promoters"
        )
        messager("Saving annotation ==>",annot_file2,v=verbose)
        saveRDS(regions, annot_file2)
    } 
    #### Set plot variables ####
    max.height <- 10
    interact_y <- (1.25 * 3) # Start after rects 
    #### Initialize plot ####
    NOTT.interact_trk <- initialize_plac_seq_plot(
        interact.DT = interact.DT,
        genomic_units = genomic_units,
        highlight_plac = highlight_plac,
        max.height = max.height,
        strip.text.y.angle = strip.text.y.angle,
        interact_y = interact_y,
        verbose = verbose)  
    #### Show enhancers/promoters as rectangles ####
    if (show_regulatory_rects) {
        NOTT.interact_trk <- add_regulatory_rects(
            NOTT.interact_trk = NOTT.interact_trk, 
            regions = regions,
            genomic_units = genomic_units,
            max.height = max.height,
            interact_y = interact_y,
            color_dict = color_dict, 
            verbose = verbose)
    }
    if (xtext == FALSE) {
        NOTT.interact_trk <- add_xtext(NOTT.interact_trk = NOTT.interact_trk,
                                       verbose = verbose)
    }
    #### Interaction track ####
    if (return_interaction_track) {
        #### Print ####
        if (show_plot){
            suppressMessages(suppressWarnings(
                methods::show(NOTT.interact_trk)
            )) 
        }
        #### Return ####
        early_return <- convert_plots(
            plot_list = list("PLACseq" = NOTT.interact_trk),
            return_as = return_as, 
            x_limits = x_limits,
            verbose = verbose)
        return(early_return)
        
    } else {
        #### Make the ggplots ####
        GWAS_trk <- create_gwas_track(dat = dat,
                                      genomic_units = genomic_units,
                                      point_size = point_size, 
                                      verbose = verbose)
        FM_trk <- create_finemap_track(dat = dat,
                                       genomic_units = genomic_units,
                                       point_size = point_size,
                                       verbose = verbose)
        #### Construct plot list ####
        plot_list <- list(
            GWAS = GWAS_trk,
            `Fine-mapping` = FM_trk,
            `Nott (2019)\nInteractome` = NOTT.interact_trk
        )
        #### Convert to desired format ####
        plot_list <- convert_plots(plot_list = plot_list, 
                                   return_as = return_as, 
                                   x_limits = x_limits, 
                                   verbose = verbose)
        if(return_as=="Tracks"){
            ## Only works for Tracks atm
            plot_list <- add_track_lines(trks = plot_list,
                                         lead.pos = lead.pos,
                                         consensus.pos = consensus.pos,
                                         verbose = verbose)
        }
        #### Save ####
        if (save_plot && !is.null(locus_dir)) {
            plot.path <- file.path(
                locus_dir,
                paste0("Nott.sn-epigenomics_ggbio.png")
            )
            messager("Saving plot ==>",plot.path,v=verbose)
            dir.create(dirname(plot.path),
                showWarnings = FALSE,
                recursive = TRUE
            )
            #### Must use ggbio to handle Tracks ####
            ggbio::ggsave(
                filename = plot.path, 
                plot = plot_list,
                height = height,
                width = width,
                dpi = dpi,
                bg = "transparent"
            )
        }
        #### Print ####
        if (show_plot){
            suppressMessages(suppressWarnings(methods::show(plot_list))) 
        }
        #### Return ####
        return(plot_list)
    }
}
