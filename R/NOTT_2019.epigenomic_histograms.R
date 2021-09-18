#' Plot brain cell-specific epigenomic data
#'
#' Brain cell-specific epigenomic data from Nott et al. (2019).
#' @keywords internal
#' @family NOTT_2019
#' @source
#' \href{https://science.sciencemag.org/content/366/6469/1134}{Nott et al. (2019)}
#' \url{https://genome.ucsc.edu/cgi-bin/hgTracks?db=hg19&lastVirtModeType=default&lastVirtModeExtraState=&virtModeType=default&virtMode=0&nonVirtPosition=&position=chr2:127770344-127983251&hgsid=778249165_ySowqECRKNxURRn6bafH0yewAiuf}
#' @examples
#' track.Nott_histo <- NOTT_2019.epigenomic_histograms(
#'     finemap_dat = echodata::BST1,
#'     locus_dir = echodata::locus_dir,
#'     save_plot = FALSE,
#'     return_assay_track = TRUE,
#'     save_annot = FALSE
#' )
#' @import ggplot2
#' @importFrom ggbio autoplot geom_rect scale_x_sequnit plotGrandLinear
#' @importFrom ggbio theme_genome ggsave
#' @importFrom stats formula
NOTT_2019.epigenomic_histograms <- function(finemap_dat,
                                            locus_dir,
                                            show_plot = TRUE,
                                            save_plot = TRUE,
                                            full_data = TRUE,
                                            return_assay_track = FALSE,
                                            binwidth = 200,
                                            density_adjust = .2,
                                            plot.zoom = "1x",
                                            strip.text.y.angle = 90,
                                            xtext = TRUE,
                                            geom = "density",
                                            plot_formula = "Cell_type ~.",
                                            fill_var = "Assay",
                                            bigwig_dir = NULL,
                                            genomic_units = "Mb",
                                            as_ggplot = TRUE,
                                            nThread = 1,
                                            save_annot = FALSE,
                                            verbose = TRUE) {
    UCSC_available <- cell_type <- P <- mean.PP <- gene <- leadSNP <-
        Consensus_SNP <- NULL

    messager("NOTT_2019:: Creating epigenomic histograms plot", v = verbose)
    # library(BiocGenerics)
    # library(GenomicRanges)
    # library(ggbio)
    # show_plot=T;save_plot=T;full_data=T;return_assay_track=F;binwidth=2500; geom="histogram"; plot_formula="Cell_type ~."; show_regulatory_rects=T;  bigwig_dir=NULL; verbose=T; nThread=1;
    # finemap_dat=echoannot::LRRK2; plot.zoom=500000; fill_var="Assay"; density_adjust=.2; strip.text.y.angle=0;

    # Import BigWig annotation files
    bigWigFiles <- echoannot::NOTT_2019.bigwig_metadata
    # Some bigWig files were initially loaded to UCSC GB, but then later taken down by the authors....
    # However I saved these files on Minerva beforehand.
    bigWigFiles <- subset(bigWigFiles, UCSC_available == "T")
    bigWigFiles <- dplyr::mutate(bigWigFiles, cell_type = gsub(" ", ".", cell_type))
    # Convert finemap data to granges
    dat <- finemap_dat
    dat$seqnames <- dat$CHR
    dat$start.end <- dat$POS
    gr.dat <- GenomicRanges::makeGRangesFromDataFrame(
        df = dat,
        seqnames.field = "seqnames",
        start.field = "start.end",
        end.field = "start.end",
        keep.extra.columns = TRUE
    )
    # ! IMPORTANT !: Needs to be in chr1 format in order to query!
    GenomeInfoDb::seqlevelsStyle(gr.dat) <- "UCSC"
    messager("NOTT_2019:: Importing bigWig subsets from UCSC...", v = verbose)
    bw.grlist <- parallel::mclapply(1:nrow(bigWigFiles), function(i) {
        if (!is.null(bigwig_dir)) {
            bw.file <- file.path(bigwig_dir, paste0(
                bigWigFiles$long_name[i],
                ".ucsc.bigWig"
            ))
        } else {
            bw.file <- bigWigFiles$data_link[i]
        }

        bw.name <- gsub("_pooled|pooled_", "", bigWigFiles$name[i])
        messager("+ NOTT_2019:: Importing...", paste0("[", i, "]"), bw.name)
        bw.filt <- import.bw.filt(
            bw.file = bw.file,
            gr.dat = gr.dat,
            full_data = full_data
        )
        bw.filt$Cell_type <- bigWigFiles$cell_type[i]
        bw.filt$Assay <- bigWigFiles$assay[i]
        bw.filt$Experiment <- gsub("_", " ", bw.name)
        return(bw.filt)
    }, mc.cores = nThread)
    bw.cols <- bigWigFiles$name
    # names(bw.grlist) <- bw.cols
    bw.gr <- unlist(GenomicRanges::GRangesList(bw.grlist))
    bw.gr$Assay <- gsub("atac", "ATAC", bw.gr$Assay)
    bw.gr$Cell_type <- gsub("oligodendrocytes", "oligo", bw.gr$Cell_type)

    xlims <- PLOT.get_window_limits(
        finemap_dat = finemap_dat,
        plot.zoom = plot.zoom,
        genomic_units = "POS"
    )
    bw.gr <- subset(
        bw.gr,
        GenomicRanges::seqnames(bw.gr) ==
            paste0("chr", gsub("chr", "", finemap_dat$CHR[1])) &
            GenomicRanges::start(bw.gr) >= xlims[1] &
            GenomicRanges::end(bw.gr) <= xlims[2]
    )
    # merge into a single granges object
    # gr.snp <- Reduce(function(x, y) GenomicRanges::merge(x, y, all.x=TRUE),
    #                  append(bw.grlist, gr.dat))
    # GenomicRanges::findOverlaps(query = gr.dat,
    #                             subject = bw.gr)

    if (save_annot) {
        annot_file <- annotation_file_name(
            locus_dir = locus_dir,
            lib_name = "Nott_2019.epigenomics"
        )
        saveRDS(bw.gr, annot_file)
    }

    PEAKS <- NOTT_2019.get_epigenomic_peaks(
        nThread = nThread,
        verbose = verbose
    )
    gr.peaks <- granges_overlap(
        dat1 = bw.gr,
        dat2 = PEAKS
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
            aes_string(fill = fill_var), show.legend = TRUE
        )
    )
    # Pause and calculate max histo height
    max_height <- PLOT.get_max_histogram_height(gg = nott_tracks)
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
            aes_string(y = "y", fill = fill_var),
            alpha = .25,
            hjust = 1,
            color = "transparent"
        ) +
        facet_grid(facets = stats::formula(plot_formula)) +
        theme_classic() +
        theme(
            legend.position = "right",
            strip.text.y = element_text(angle = strip.text.y.angle)
        ) +
        scale_y_continuous(n.breaks = 3) +
        scale_fill_manual(values = color_dict)

    #### Make adjustments ####
    if (genomic_units == "Mb") {
        messager("++ Converting label units to Mb...", v = verbose)
        nott_tracks <- suppressMessages(
            nott_tracks +
                ggbio::scale_x_sequnit(unit = "Mb")
        )
    }
    if (xtext == F) {
        nott_tracks <- nott_tracks +
            theme(
                axis.text.x = element_blank(),
                axis.title.x = element_blank()
            )
    }
    if (return_assay_track) {
        if (as_ggplot) {
            return(nott_tracks@ggplot)
        } else {
            return(nott_tracks)
        }
    }



    #### (optional) Fine-mapping tracks ####
    TRACKS_list <- list(
        "GWAS" = ggbio::plotGrandLinear(obj = gr.dat, aes(y = -log10(P), color = -log10(P))),
        "Fine_mapping" = ggbio::plotGrandLinear(obj = gr.dat, aes(y = mean.PP, color = mean.PP)) +
            scale_color_viridis_c(),
        "Nott_etal_2019" = nott_tracks
    )
    # Fuse all tracks
    params_list <- list(
        title = paste0(gene, " [", length(GenomicRanges::seqnames(gr.dat)), " SNPs]"),
        track.bg.color = "transparent",
        track.plot.color = "transparent",
        label.text.cex = .7,
        label.bg.fill = "grey12",
        label.text.color = "white",
        label.text.angle = 0,
        label.width = unit(5.5, "lines"),
        # xlim = c(min(start(gr.snp)), max(start(gr.snp))),
        heights = c(.3, .3, 1)
    )
    tracks <- get("tracks", asNamespace("ggbio"))
    trks <- suppressWarnings(do.call("tracks", append(TRACKS_list, params_list)))
    # add lines
    lead.pos <- subset(finemap_dat, leadSNP)$POS
    consensus.pos <- subset(finemap_dat, Consensus_SNP == TRUE)$POS

    trks_plus_lines <- trks +
        # theme_bw() +
        # ggbio::theme_genome() +
        theme(
            strip.text.y = element_text(angle = 0),
            strip.text = element_text(size = 9),
            panel.background = element_rect(fill = "white", colour = "black", linetype = "solid")
        ) +
        geom_vline(xintercept = consensus.pos, color = "goldenrod2", alpha = 1, size = .1, linetype = "solid") +
        geom_vline(xintercept = lead.pos, color = "red", alpha = 1, size = .1, linetype = "solid")

    if (show_plot) {
        print(trks_plus_lines)
    }
    if (save_plot) {
        save_path <- file.path(
            locus_dir, "annotations",
            paste0(basename(locus_dir), "_Glass.snEpigenomics.png")
        )
        dir.create(dir(save_path), showWarnings = F, recursive = T)
        ggbio::ggsave(save_path,
            plot = trks_plus_lines, dpi = 400, height = 15, width = 8,
            bg = "transparent"
        )
    }
    if (as_ggplot) {
        return(trks_plus_lines@ggplot)
    } else {
        return(trks_plus_lines)
    }
}
