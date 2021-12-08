#' Plot brain cell-specific epigenomic data
#'
#' Brain cell-specific epigenomic data from Nott et al. (2019).
#' 
#' @param dat Fine-mapping results data from \link[echolocatoR]{finemap_loci}.
#' @inheritParams ggbio::ggsave
#' 
#' @family NOTT2019
#' @source
#' \href{https://science.sciencemag.org/content/366/6469/1134}{Nott et al. (2019)}
#' \url{https://genome.ucsc.edu/cgi-bin/hgTracks?db=hg19&lastVirtModeType=default&lastVirtModeExtraState=&virtModeType=default&virtMode=0&nonVirtPosition=&position=chr2:127770344-127983251&hgsid=778249165_ySowqECRKNxURRn6bafH0yewAiuf}
#' 
#' @export
#' @importFrom stats formula
#' @importFrom echodata dt_to_granges
#' @importFrom GenomicRanges mcols
#' @examples
#' nott2019_track <- echoannot::NOTT2019_epigenomic_histograms(
#'     dat = echodata::BST1)
NOTT2019_epigenomic_histograms <- function(dat,
                                           locus_dir = tempdir(),
                                           show_plot = TRUE,
                                           save_plot = FALSE,
                                           full_data = TRUE,
                                           bigwig_dir = NULL,
                                           return_assay_track = FALSE,
                                           binwidth = 200,
                                           density_adjust = .2,
                                           plot.zoom = "1x",
                                           strip.text.y.angle = 90,
                                           xtext = TRUE,
                                           geom = "density",
                                           plot_formula = "Cell_type ~.",
                                           fill_var = "Assay", 
                                           genomic_units = "Mb",
                                           as_ggplot = TRUE,
                                           dpi = 300, 
                                           height = 15, 
                                           width = 8,
                                           nThread = 1,
                                           save_annot = FALSE,
                                           verbose = TRUE) {  
    # show_plot=T;save_plot=T;full_data=T;return_assay_track=F;
    # binwidth=2500; geom="histogram"; plot_formula="Cell_type ~.";
    # show_regulatory_rects=T;  bigwig_dir=NULL; verbose=T; nThread=1;
    # plot.zoom=500000; fill_var="Assay";plot.zoom = "1x";
    # density_adjust=.2; strip.text.y.angle=0; dat <- echodata::BST1;
    # save_annot=T; locus_dir <- tempdir(); fill_var="Assay";
    # genomic_units="Mb"; save_path <- tempfile()
    
    requireNamespace("ggplot2")
    requireNamespace("ggbio")
    UCSC_available <- cell_type <- P <- mean.PP <- gene <- leadSNP <-
        Consensus_SNP <- NULL

    messager("NOTT2019:: Creating epigenomic histograms plot",
             v = verbose)
    #### Import BigWig annotation metadata ####
    bigwig_metadata <- echoannot::NOTT2019_bigwig_metadata
    # Some bigWig files were initially loaded to UCSC GB,
    # but then later taken down by the authors....
    # However I saved these files on Minerva beforehand.
    bigwig_metadata <- subset(bigwig_metadata, UCSC_available == "T")
    bigwig_metadata <- dplyr::mutate(bigwig_metadata,
                                     cell_type = gsub(" ", ".", cell_type))
    #### Create save path name ####
    annot_file <- annotation_file_name(
        locus_dir = locus_dir,
        lib_name = "NOTT2019_epigenomics"
    )
    #### Prepare query data ####
    #### Convert fine-map data to granges #### 
    # ! IMPORTANT !: Needs to be in chr1 format in order to query! 
    gr.dat <- echodata::dt_to_granges(dat = dat,
                                      chrom_col = "CHR",
                                      start_col = "POS",
                                      style = "UCSC",
                                      verbose = FALSE)  
    #### Get min/max genomic coordinates ####
    xlims <- get_window_limits(
        dat = dat,
        plot.zoom = plot.zoom,
        genomic_units = "POS"
    )
    ##### Import data from UCSC ####
    bw.gr <- import_ucsc_bigwigs(query_dat = dat,
                                 bigwig_metadata = bigwig_metadata,
                                 full_data = full_data,
                                 xlims = xlims, 
                                 save_path = if(save_annot) {
                                     save_plot
                                 } else {NULL}, 
                                 force_new = FALSE, 
                                 nThread = nThread, 
                                 verbose = verbose) 
    #### Additional preprocessing ####
    GenomicRanges::mcols(bw.gr)["Assay"] <- 
        gsub("atac", "ATAC", bw.gr$Assay)
    GenomicRanges::mcols(bw.gr)["Cell_type"] <- 
        gsub("oligodendrocytes", "oligo", bw.gr$Cell_type)
    #### Get previously called peaks ####
    PEAKS <- NOTT2019_get_epigenomic_peaks(
        nThread = nThread,
        verbose = verbose
    )
    gr.peaks <- granges_overlap(
        dat1 = bw.gr,
        dat2 = PEAKS, 
        chrom_col.2 = "chr",
        start_col.2 = "start",
        end_col.2 = "end"
    )
    GenomicRanges::mcols(gr.peaks)[, c("chr", "start", "end", "score")] <- NULL
    gr.peaks <- unique(gr.peaks)
    # Adjust line width to make sure bars aren't just all white
    line_width <- 0 # binwidth/1000 * .25

    #### Density/Histogram plot ####
    color_dict <- assay_color_dict()
    nott_tracks <- suppressWarnings(
        ggbio::autoplot(
            object = bw.gr,
            geom = geom,
            binwidth = binwidth,
            alpha = .7,
            position = "stack",
            adjust = density_adjust,
            color = "white",
            size = line_width,
            ggplot2::aes_string(fill = fill_var), 
            show.legend = TRUE
        )
    )
    #### Pause and calculate max histo height ####
    max_height <- get_max_histogram_height(gg = nott_tracks)
    rect_height <- max_height / if (geom == "density") 8 else 10
    gr.peaks$y <- 0 - rect_height
    if (gsub(" ", "", plot_formula) == "Cell_type~.") {
        gr.peaks$Assay <- "peaks"
    }
    nott_tracks <- nott_tracks +
        ggbio::geom_rect(gr.peaks,
            stat = "identity",
            # position="stack",
            rect.height = rect_height,
            ggplot2::aes_string(y = "y", fill = fill_var),
            alpha = .25,
            hjust = 1,
            color = "transparent"
        ) +
        ggplot2::facet_grid(facets = stats::formula(plot_formula)) +
        ggplot2::theme_classic() +
        ggplot2::theme(
            legend.position = "right",
            strip.text.y = ggplot2::element_text(angle = strip.text.y.angle)
        ) +
        ggplot2::scale_y_continuous(n.breaks = 3) +
        ggplot2::scale_fill_manual(values = color_dict)

    #### Make adjustments ####
    if (genomic_units == "Mb") {
        messager("+ Converting label units to Mb.", v = verbose)
        nott_tracks <- suppressMessages(
            nott_tracks +
                ggbio::scale_x_sequnit(unit = "Mb")
        )
    }
    if (xtext == FALSE) {
        nott_tracks <- nott_tracks +
            ggplot2::theme(
                axis.text.x = ggplot2::element_blank(),
                axis.title.x = ggplot2::element_blank()
            )
    }
    #### Return 1 ####
    if (return_assay_track) {
        if (as_ggplot) {
            return(list(data=list(raw=bw.gr,
                                  peaks=gr.peaks),
                        plot=nott_tracks@ggplot))
        } else {
            return(list(data=list(raw=bw.gr,
                                  peaks=gr.peaks),
                        plot=nott_tracks))
        }
    } 
    #### (optional) Fine-mapping tracks ####
    TRACKS_list <- list(
        "GWAS" = ggbio::plotGrandLinear(
            obj = gr.dat, 
            ggplot2::aes(y = -log10(P), color = -log10(P))),
        "Fine_mapping" = ggbio::plotGrandLinear(
            obj = gr.dat, 
            ggplot2::aes(y = mean.PP, color = mean.PP)) +
            ggplot2::scale_color_viridis_c(),
        "Nott_etal_2019" = nott_tracks
    )
    # Fuse all tracks
    params_list <- list(
        title = paste0(gene, 
                       " [", formatC(
                           length(GenomicRanges::seqnames(gr.dat)),
                           big.mark = ","), 
                       " SNPs]"),
        track.bg.color = "transparent",
        track.plot.color = "transparent",
        label.text.cex = .7,
        label.bg.fill = "grey12",
        label.text.color = "white",
        label.text.angle = 0,
        label.width = ggplot2::unit(5.5, "lines"),
        # xlim = c(min(start(gr.snp)), max(start(gr.snp))),
        heights = c(.3, .3, 1)
    )
    tracks <- get("tracks", asNamespace("ggbio"))
    trks <- suppressMessages(do.call("tracks",
                                     append(TRACKS_list, params_list)))
    #### Add lines ####
    lead.pos <- subset(dat, leadSNP)$POS
    consensus.pos <- subset(dat, Consensus_SNP == TRUE)$POS

    trks_plus_lines <- trks + 
        ggplot2::theme(
            strip.text.y = ggplot2::element_text(angle = 0),
            strip.text = ggplot2::element_text(size = 9),
            panel.background = ggplot2::element_rect(fill = "white",
                                                     colour = "black", 
                                                     linetype = "solid")
        ) +
        ggplot2::geom_vline(xintercept = consensus.pos, 
                   color = "goldenrod2", alpha = 1, size = .1,
                   linetype = "solid") +
        ggplot2::geom_vline(xintercept = lead.pos, 
                   color = "red", alpha = 1, size = .1, 
                   linetype = "solid")

    if (show_plot) {
        print(trks_plus_lines)
    }
    if (save_plot) {
        save_path <- file.path(
            locus_dir, "annotations",
            paste0(basename(locus_dir), "_Glass.snEpigenomics.png")
        )
        dir.create(dir(save_path), showWarnings = FALSE, recursive = TRUE)
        ggbio::ggsave(save_path,
            plot = trks_plus_lines, 
            dpi = dpi, 
            height = height, 
            width = width,
            bg = "transparent"
        )
    }
    #### Return 2 ####
    if (as_ggplot) {
        gg_list <- tracks_to_ggplot_list(trks = trks_plus_lines,
                                         verbose = verbose)
        return(list(data=list(raw=bw.gr,
                              peaks=gr.peaks),
                    plot=gg_list))
    } else {
        return(list(data=list(raw=bw.gr,
                              peaks=gr.peaks),
                    plot=trks_plus_lines))
    }
}
