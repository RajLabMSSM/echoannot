#' Download XGR annotations
#'
#' @family XGR
#' @keywords internal
#' @importFrom  XGR xRDataLoader
XGR.import_annotations <- function(gr.snp,
                                   anno_data_path =
                                       file.path(
                                           "annotations",
                                           paste0(
                                               "XGR_",
                                               lib.name, ".rds"
                                           )
                                       ),
                                   lib.name,
                                   save_xgr = TRUE,
                                   annot_overlap_threshold = 5) {
    if (file.exists(anno_data_path)) {
        messager("")
        messager("+ Saved annotation file detected. Loading...")
        GR.annotations <- readRDS(anno_data_path)
    } else {
        messager("")
        messager("+ XGR: Downloading...", lib.name)
        GR.annotations <- XGR::xRDataLoader(RData.customised = lib.name)
        if (save_xgr & !is.null(GR.annotations)) {
            dir.create(dirname(anno_data_path),
                showWarnings = FALSE, recursive = TRUE
            )
            saveRDS(GR.annotations, file = anno_data_path)
        }
    }
    GR.orig <- unlist(GR.annotations)

    gr.xgr <- lapply(names(GR.orig), function(g, gr.snp. = gr.snp) {
        # messager("Finding overlap for:", g)
        subsetByOverlaps <- get(
            "subsetByOverlaps",
            asNamespace("GenomicRanges")
        )
        GR.overlap <- subsetByOverlaps(GR.orig[[g]], gr.snp.)
        len <- length(seqnames(GR.overlap))
        # messager("   - Overlapping annotations = ",len)
        if (len > 0) {
            return(GR.overlap)
        } else {
            return(NULL)
        }
    })
    grl.xgr <- GR.name_filter_convert(gr.xgr,
        names(GR.orig),
        min_hits = annot_overlap_threshold
    )
    return(grl.xgr)
}
