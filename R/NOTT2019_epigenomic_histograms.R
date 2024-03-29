#' Plot brain cell-specific epigenomic data
#'
#' Brain cell-specific epigenomic data from Nott et al. (2019).
#' 
#' @param dat Fine-mapping results data from \link[echolocatoR]{finemap_loci}.
#' @param locus_dir Locus-specific directory.
#' @param show_plot Show plot.
#' @param save_plot Whether to save the plot. 
#' @param return_assay_track Return only the assay track 
#' (before adding the rest of the tracks and showing the plot).
#' @param density_adjust Passed to \code{adjust} argument in 
#' \link[ggplot2]{geom_density}.
#' @param xtext Whether to include x-axis title and text. 
#' @param plot_formula Formula passed to \code{facets} argument in 
#' \link[ggplot2]{facet_grid}. 
#' @param fill_var Variable name to use for plot \code{fill} argument.
#' @param as_ggplot Return plot as \code{ggplot2} 
#' (\code{TRUE}) or \code{Tracks} (\code{FALSE}) object. 
#' @param save_annot Save the queried subset of bigwig annotations.
#' @param strip.text.y.angle Angle of the y-axis facet labels. 
#' 
#' @inheritParams ggbio::ggsave
#' @inheritParams ggplot2::theme
#' @inheritParams ggbio::autoplot
#' @inheritParams import_ucsc_bigwigs
#' @inheritParams get_window_limits
#' 
#' @family NOTT2019
#' @source
#' \href{https://doi.org/10.1126/science.aay0793}{Nott et al. (2019)}
#'
#' 
#' @export
#' @importFrom stats formula
#' @importFrom echodata dt_to_granges
#' @importFrom GenomicRanges mcols
#' @importFrom methods show
#' @examples
#' nott2019_track <- echoannot::NOTT2019_epigenomic_histograms(
#'     dat = echodata::BST1, 
#'     bigwig_metadata = echoannot::NOTT2019_bigwig_metadata[1:2,])
NOTT2019_epigenomic_histograms <- function(
        dat,
        bigwig_metadata = echoannot::NOTT2019_bigwig_metadata,
        locus_dir = tempdir(),
        show_plot = TRUE,
        save_plot = FALSE,
        full_data = TRUE,
        return_assay_track = FALSE,
        binwidth = 200,
        density_adjust = .2,
        zoom = "1x",
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
     
    # echoverseTemplate:::source_all(packages = "dplyr")
    # echoverseTemplate:::args2vars(NOTT2019_epigenomic_histograms)
    requireNamespace("ggplot2")
    requireNamespace("ggbio")
    UCSC_available <- cell_type <- P <- mean.PP <- leadSNP <-
        Consensus_SNP <- NULL

    messager("NOTT2019:: Creating epigenomic histograms plot",
             v = verbose)
    #### Import BigWig annotation metadata #### 
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
        zoom = zoom,
        genomic_units = "POS"
    )
    ##### Import data from UCSC ####
    bw.gr <- import_ucsc_bigwigs(query_dat = dat,
                                 bigwig_metadata = bigwig_metadata,
                                 full_data = full_data,
                                 xlims = xlims, 
                                 save_path = if(save_annot) {
                                     file.path(
                                         tempdir(),
                                         paste(basename(locus_dir),
                                               "Nott2019_bigwig.rds",
                                               sep="_"))
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
            # hjust = 1,
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
    locus_name <- if(locus_dir!=tempdir()) basename(locus_dir) else NULL
    params_list <- list(
        title = paste0(locus_name, 
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
        methods::show(trks_plus_lines)
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
        gg_list <- tracks_to_ggplot_list(trks = trks_plus_lines)
        return(list(data=list(raw=bw.gr,
                              peaks=gr.peaks),
                    plot=gg_list))
    } else {
        return(list(data=list(raw=bw.gr,
                              peaks=gr.peaks),
                    plot=trks_plus_lines))
    }
}
