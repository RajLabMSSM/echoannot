#' Plot brain cell-specific interactome data
#'
#' Plot brain cell-specific interactome data from
#'  \href{https://science.sciencemag.org/content/366/6469/1134}{Nott et al. (2019)}.
#'
#' @family NOTT2019
#' @source
#' \href{https://science.sciencemag.org/content/366/6469/1134}{Nott et al. (2019)}
#' \url{https://genome.ucsc.edu/cgi-bin/hgTracks?db=hg19&lastVirtModeType=default&lastVirtModeExtraState=&virtModeType=default&virtMode=0&nonVirtPosition=&position=chr2:127770344-127983251&hgsid=778249165_ySowqECRKNxURRn6bafH0yewAiuf}
#'
#' @export
#' @importFrom IRanges IRanges
#' @importFrom echodata find_consensus_snps
#' @importFrom dplyr %>% top_n
#' @importFrom GenomicRanges seqnames start end GRanges findOverlaps
#' @importFrom S4Vectors subjectHits
#' @examples
#' dat <- echodata::BST1
#' locus_dir <- file.path(tempdir(), echodata::locus_dir)
#' trks_plus_lines <- echoannot::NOTT2019_plac_seq_plot(
#'     dat = dat,
#'     locus_dir = locus_dir,
#'     highlight_plac = TRUE
#' ) 
NOTT2019_plac_seq_plot <- function(dat = NULL,
                                   locus_dir = NULL,
                                   title = NULL,
                                   print_plot = TRUE,
                                   save_plot = TRUE,
                                   return_interaction_track = FALSE,
                                   xlims = NULL,
                                   zoom_window = NULL,
                                   index_SNP = NULL,
                                   genomic_units = "Mb",
                                   color_dict = c(
                                       "enhancers" = "springgreen2",
                                       "promoters" = "purple",
                                       "anchors" = "black"
                                   ),
                                   return_consensus_overlap = TRUE,
                                   show_arches = TRUE,
                                   highlight_plac = FALSE,
                                   show_regulatory_rects = TRUE,
                                   show_anchors = TRUE,
                                   strip.text.y.angle = 0,
                                   xtext = TRUE,
                                   save_annot = FALSE,
                                   point_size = 2,
                                   height = 7,
                                   width = 7,
                                   dpi = 300,
                                   as_ggplot = TRUE,
                                   nThread = 1,
                                   verbose = TRUE) {
    # dat=echoannot::LRRK2; print_plot=T; save_plot=T; title=NULL;
    # xlims=NULL; zoom_window=NULL; return_consensus_overlap =T; nThread=1;
    # highlight_plac=F; point_size=2;  index_SNP=NULL;
    # color_dict=c("enhancers"="springgreen",
    #              "promoters"="purple","anchors"="black");
    # genomic_units="Mb"; verbose=T; save_annot=F;
    
    requireNamespace("ggplot2")
    requireNamespace("ggbio")
    leadSNP <- SNP <- Consensus_SNP <- mean.PP <- Effect <- Support <-
        Start <- End <- consensus_snp_overlap <- y <- Element <-
        POS <- P <- NULL;
    messager("NOTT2019:: Creating PLAC-seq interactome plot", v = verbose)
    if (!"Mb" %in% colnames(dat)) {
        dat$Mb <- dat$POS / 1000000
    }
    xvar <- genomic_units
    if (!"Consensus_SNP" %in% colnames(dat)) {
        dat <-
            echodata::find_consensus_snps(
                dat = dat,
                verbose = FALSE
            )
    }
    marker_key <- list(
        PU1 = "microglia", Olig2 = "oligo",
        NeuN = "neurons", LHX2 = "astrocytes"
    )
    if (is.null(index_SNP)) {
        lead.pos <- subset(dat, leadSNP)[[xvar]]
    } else {
        lead.pos <- subset(dat, SNP == index_SNP)[[xvar]]
    }
    consensus.pos <- subset(dat, Consensus_SNP == TRUE)[[xvar]]

    if (length(consensus.pos) > 0) {
        top.consensus.pos <-
            (dplyr::top_n(subset(dat, Consensus_SNP == TRUE),
                n = 1, wt = mean.PP
            ) %>%
                dplyr::top_n(1, wt = Effect))[[xvar]][1]
    } else {
        top.consensus.pos <-
            (dplyr::top_n(subset(dat, Support > 0),
                n = 1, wt = mean.PP
            ) %>%
                dplyr::top_n(1, wt = Effect))[[xvar]][1]
    }
    if (is.null(xlims)) {
        xlims <- c(
            min(dat[[xvar]], na.rm = TRUE),
            max(dat[[xvar]], na.rm = TRUE)
        )
    }
    if (!is.null(zoom_window)) {
        xlims <- c(lead.pos - as.integer(zoom_window / 2), lead.pos +
            as.integer(zoom_window / 2))
    }
    annot_sub <- NOTT2019_get_promoter_interactome_data(dat = dat)
    promoter_celltypes <- NOTT2019_get_promoter_celltypes(
        annot_sub = annot_sub,
        marker_key = marker_key
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
        as.granges = TRUE,
        verbose = verbose
    )
    #### regions needs to be in ####
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
            as.granges = TRUE
        )
        interact.anchors$Element <- "anchors"
        regions <- c(regions, interact.anchors)
    }
    
    ##### JH - which PLAC-Seq junctions overlap (5kb) the consensus SNPs? ####
    if (highlight_plac) { 
        # interact.DT
        consensus_snps <- subset(dat, Consensus_SNP == TRUE)
        consensus_snps$CHR <- paste0("chr", consensus_snps$CHR)
        # make GRanges
        consensus.gr <- GenomicRanges::GRanges(
            seqnames = consensus_snps$CHR,
            ranges = IRanges::IRanges(
                start = consensus_snps$POS - 1,
                end = consensus_snps$POS
            )
        )
        plac_start.gr <- GenomicRanges::GRanges(
            seqnames = interact.DT$chr,
            ranges = IRanges::IRanges(
                start = interact.DT$Start - 5000,
                end = interact.DT$Start
            )
        )
        plac_end.gr <- GenomicRanges::GRanges(
            seqnames = interact.DT$chr,
            ranges = IRanges::IRanges(
                start = interact.DT$End,
                end = interact.DT$End + 5000
            )
        )
        #### find overlaps ####
        end_overlaps <- GenomicRanges::findOverlaps(
            consensus.gr,
            plac_end.gr
        )
        start_overlaps <- GenomicRanges::findOverlaps(
            consensus.gr,
            plac_start.gr
        )
        all_overlaps <- unique(c(
            S4Vectors::subjectHits(end_overlaps),
            S4Vectors::subjectHits(start_overlaps)
        )) 
        interact.DT$consensus_snp_overlap <- FALSE
        interact.DT$consensus_snp_overlap[all_overlaps] <- TRUE
    }
    #### save annotations ####
    if (save_annot) {
        annot_file <- annotation_file_name(
            locus_dir = locus_dir,
            lib_name = "NOTT2019_interactome"
        )
        saveRDS(interact.DT, annot_file)
        annot_file <- annotation_file_name(
            locus_dir = locus_dir,
            lib_name = "NOTT2019_enhancers_promoters"
        )
        saveRDS(regions, annot_file)
    } 
    max.height <- 10
    interact_y <- (1.25 * 3) # Start after rects 

    if (highlight_plac) {
        NOTT.interact_trk <-
            ggbio::ggbio() +
            ggbio::geom_arch(
                data = interact.DT,
                ggplot2::aes(
                    x = Start,
                    xend = End,
                    alpha = consensus_snp_overlap
                ),
                max.height = max.height, colour = "black"
            ) +
            ggplot2::scale_alpha_manual("Consensus SNP overlaps", 
                                        values = c(0.05, 1))
    } else {
        NOTT.interact_trk <-
            ggbio::ggbio() +
            ggbio::geom_arch(
                data = interact.DT, alpha = 0.25, color = "black",
                max.height = max.height,
                ggplot2::aes(x = Start, xend = End, y = interact_y)
            ) +
            ggplot2::labs(y = NULL)
    }

    NOTT.interact_trk <- suppressMessages(
        NOTT.interact_trk +
            ggplot2::facet_grid(facets = Cell_type ~ .) +
            ggplot2::scale_y_reverse() +
            ggplot2::theme_classic() +
            ggplot2::theme(
                legend.key.width = ggplot2::unit(1.5, "line"),
                legend.key.height = ggplot2::unit(1.5, "line"),
                axis.text.y = ggplot2::element_blank(),
                strip.text.y = ggplot2::element_text(
                    angle = strip.text.y.angle)
            )
    )
 
    #### Show enhancers/promoters as rectangles ####
    if (show_regulatory_rects) {
        messager("++ NOTT2019:: Adding enhancer/promoter rectangles",
                 v = verbose)
        rect_height <- max.height / 10
        regions$y <- ifelse(regions$Element == "promoters",
                            0 + (rect_height * 2),
            ifelse(regions$Element == "enhancers", 0,
                interact_y + rect_height
            )
        )
        regions$Element <- factor(regions$Element,
            levels = c("enhancers", "promoters", "anchors"),
            ordered = TRUE
        )

        NOTT.interact_trk <- suppressMessages(
            NOTT.interact_trk +
                ggbio::geom_rect(
                    data = regions,
                    stat = "identity",
                    rect.height = rect_height,
                    ggplot2::aes(y = y, fill = Element),
                    alpha = .7, inherit.aes = FALSE,
                    color = "transparent",
                    hjust = 0
                ) +
                ggplot2::scale_fill_manual(values = color_dict) +
                # geom_point(data = data.frame(regions),
                #            aes(x = middle, y = 0, color = Element),size = 0.5,
                #            inherit.aes = F, alpha = 0.7) +
                ggplot2::scale_color_manual(values = color_dict) +
                ggplot2::geom_hline(
                    yintercept = Inf, alpha = 0.2,
                    show.legend = FALSE
                ) +
                ggplot2::scale_y_reverse()
        )

        if (genomic_units == "Mb") {
            messager("++ Converting genomic units to Mb...", v = verbose)
            NOTT.interact_trk <- NOTT.interact_trk +
                ggbio::scale_x_sequnit(unit = "Mb")
        }
    }
    if (xtext == F) {
        NOTT.interact_trk <- NOTT.interact_trk +
            ggplot2::theme(
                axis.text.x = ggplot2::element_blank(),
                axis.title.x = ggplot2::element_blank()
            )
    }

    if (return_interaction_track) {
        messager("++ NOTT2019:: Returning PLAC-seq track.")
        if (as_ggplot) {
            return(NOTT.interact_trk@ggplot)
        } else {
            return(NOTT.interact_trk)
        }
    } else {
        # make the ggplots
        GWAS_trk <- ggplot2::ggplot(
            data = dat,
            ggplot2::aes(x = POS, y = -log10(P), color = -log10(P))
        ) +
            ggplot2::geom_point(alpha = .5, shape = 16, size = point_size)

        FM_trk <- ggplot2::ggplot(
            data = dat,
            ggplot2::aes(x = POS, y = mean.PP, color = mean.PP)
        ) +
            ggplot2::geom_point(alpha = .5, shape = 16, size = point_size) +
            ggplot2::scale_color_viridis_c(
                breaks = c(0, 0.5, 1),
                limits = c(0, 1)
            ) +
            ggplot2::ylim(0, 1)

        TRACKS_list <- list(
            GWAS = GWAS_trk + ggplot2::theme_classic(),
            `Fine-mapping` = FM_trk + ggplot2::theme_classic(),
            `Nott (2019)\nInteractome` = NOTT.interact_trk
        )
        params_list <- list(
            title = paste0(title),
            track.bg.color = "transparent",
            track.plot.color = "transparent",
            label.text.cex = 0.7,
            label.bg.fill = "grey12",
            label.text.color = "white",
            label.text.angle = 0,
            label.width = ggplot2::unit(5.5, "lines"),
            xlim = xlims
        )
        TRACKS_list <- append(TRACKS_list, params_list)
        tracks <- get("tracks", asNamespace("ggbio"))
        trks <- suppressWarnings(do.call("tracks", TRACKS_list))
        trks_plus_lines <- trks +
            ggplot2::geom_vline(
                xintercept = lead.pos,
                color = "red", alpha = 1, size = 0.3,
                linetype = "solid"
            ) +
            ggplot2::geom_vline(
                xintercept = consensus.pos, color = "goldenrod2",
                alpha = 1, size = 0.3, linetype = "solid"
            )
        if (print_plot) {
            print(trks_plus_lines)
        }

        if (save_plot) {
            plot.path <- file.path(
                locus_dir,
                paste0("Nott.sn-epigenomics_ggbio.png")
            )
            dir.create(dirname(plot.path),
                showWarnings = FALSE,
                recursive = TRUE
            )
            ggbio::ggsave(
                filename = plot.path, plot = trks_plus_lines,
                height = height,
                width = width,
                dpi = dpi,
                bg = "transparent"
            )
        }
        #### Return ####
        if (as_ggplot) {
            return(trks_plus_lines@ggplot)
        } else {
            return(trks_plus_lines)
        }
    }
}