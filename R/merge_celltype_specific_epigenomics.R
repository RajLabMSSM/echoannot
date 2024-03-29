#' Merge all cell-type-specific epigenomics
#'
#' Merges multiple cell-type-specific epigenomic datasets
#' (Nott 2019, Corces 2020) into a single \link[GenomicRanges]{GRanges} object.
#' @param keep_extra_cols Keep extra columns
#' that are not shared across all annotations.
#' @param save_path Path to save merged results to.
#' @param force_new If cached merged results already exist, ignore them
#' and recreate the file anyway. 
#' @param verbose Print messages.
#' @export
#' @importFrom tidyr separate
#' @importFrom dplyr mutate select
#' @importFrom data.table rbindlist data.table 
#' @importFrom echotabix liftover
#' @importFrom tools R_user_dir
#' @examples
#' gr.merged <- echoannot::merge_celltype_specific_epigenomics()
merge_celltype_specific_epigenomics <- function(keep_extra_cols = FALSE,
                                                save_path = file.path(
                                                    tools::R_user_dir(
                                                        package = "echoannot",
                                                        which = "cache"),
                                    "merge_celltype_specific_epigenomics.rds"
                                                ),
                                    force_new = FALSE,
                                    verbose = TRUE
                                                ) {
    
    id <- Cell_type <- Peak_ID <- Study <- Assay <- NULL 
    
    if(file.exists(save_path) && isFALSE(force_new)){
        messager("Importing pre-processed gr.merged",v=verbose)
        gr.merged <- readRDS(save_path)
        return(gr.merged)
    } 
    #### NOTT 2019 ####
    ## Peaks
    gr.Nott2019.peaks <- NOTT2019_get_epigenomic_peaks(
        convert_to_granges = TRUE,
        nThread = 1
    )
    gr.Nott2019.peaks$Study <- "Nott2019.celltype_peaks"
    ### Regulatory regions
    gr.Nott2019.regions <- NOTT2019_get_regulatory_regions(as_granges = TRUE)
    gr.Nott2019.regions$Study <- "Nott2019.celltype_regions"
    gr.Nott2019.regions$Assay <- gr.Nott2019.regions$Element
    ### Interactome
    NOTT2019_interactome <- get_NOTT2019_interactome()
    interactome <- NOTT2019_interactome[
        grep("interactome", names(NOTT2019_interactome))
    ] |>
        data.table::rbindlist(idcol = "id") |>
        tidyr::separate(id,
            sep = " ",
            remove = FALSE,
            into = c("Cell_type", "Data_type")
        ) |>
        dplyr::mutate(
            Cell_type = standardize_celltypes(Cell_type),
            Assay = "PLAC", Study = "Nott2019.celltype_interactome"
        )
    gr.Nott2019.interactome <- c(
        echodata::dt_to_granges(
            dat = interactome |> dplyr::mutate(Anchor = 1),
            chrom_col = "chr1", 
            start_col = "start1",
            end_col = "end1", 
            style = "NCBI"
        ),
        echodata::dt_to_granges(
            dat = interactome |> dplyr::mutate(Anchor = 2),
            chrom_col = "chr2", 
            start_col = "start2",
            end_col = "end2", 
            style = "NCBI"
        )
    )  
    #### CORCES 2020 ####
    ## Peaks
    gr.Corces2020.peaks <- CORCES2020_scATAC_to_granges(
        standardize_cellTypes = TRUE
    )
    gr.Corces2020.peaks$Study <- "Corces2020.celltype_peaks"

    ## Cicero Interactome
    #### Assign cell types to interactome
    cicero <- get_CORCES2020_cicero_coaccessibility() |>
        dplyr::mutate(
            Study = "Corces2020.celltype_interactome",
            Assay = "cicero"
        )
    #### Convert anchor 1 ####
    cicero.anchor1 <- cicero |>
        data.table::merge.data.table(
            y = data.table::data.table(data.frame(gr.Corces2020.peaks)) |>
                dplyr::select(Peak_ID, Cell_type),
        by.x = "Peak_ID_Peak1",
        by.y = "Peak_ID"
        ) |>
        echotabix::liftover(
            query_genome = "hg38",
            target_genome = "hg19",
            query_chrom_col = "hg38_Chromosome_Peak1",
            query_start_col = "hg38_Start_Peak1",
            query_end_col = "hg38_Stop_Peak1",
            as_granges = TRUE,
            style = "NCBI"
        )
    #### Convert anchor 2 ####
    cicero.anchor2 <- cicero |>
        data.table::merge.data.table(data.table::data.table(
            data.frame(gr.Corces2020.peaks)
        ) |>
            dplyr::select(Peak_ID, Cell_type),
        by.x = "Peak_ID_Peak2",
        by.y = "Peak_ID"
        ) |>
        echotabix::liftover(
            query_genome = "hg38",
            target_genome = "hg19",
            query_chrom_col = "hg38_Chromosome_Peak2",
            query_start_col = "hg38_Start_Peak2",
            query_end_col = "hg38_Stop_Peak2",
            as_granges = TRUE,
            style = "NCBI"
        )
    gr.Corces2020.cicero <- c(cicero.anchor1, cicero.anchor2)


    ### Bulk ATACseq peaks
    gr.Corces2020.bulk_peaks <-
        get_CORCES2020_bulkATACseq_peaks() |>
        dplyr::mutate(
            Study = "Corces2020.bulk_peaks",
            Assay = "ATAC",
            Cell_type = "brain"
        ) |>
        echotabix::liftover(
            query_genome = "hg38",
            target_genome = "hg19",
            query_chrom_col = "hg38_Chromosome",
            query_start_col = "hg38_Start",
            query_end_col = "hg38_Stop",
            as_granges = TRUE,
            style = "NCBI"
        )

    ### FitChip interactome
    fitchip <- get_CORCES2020_hichip_fithichip_loop_calls() |>
        dplyr::mutate(
            Study = "Corces2020.bulk_interactome",
            Cell_type = "brain", Assay = "HiChIP_FitHiChIP"
        )
    fitchip.anchor1 <- fitchip |>
        dplyr::mutate(Anchor = 1) |>
        echotabix::liftover(
            query_genome = "hg38",
            target_genome = "hg19",
            query_chrom_col = "hg38_Chromosome_Anchor1",
            query_start_col = "hg38_Start_Anchor1",
            query_end_col = "hg38_Stop_Anchor1",
            as_granges = TRUE,
            style = "NCBI"
        )
    fitchip.anchor2 <- fitchip |>
        dplyr::mutate(Anchor = 2) |>
        echotabix::liftover(
            query_genome = "hg38",
            target_genome = "hg19",
            query_chrom_col = "hg38_Chromosome_Anchor2",
            query_start_col = "hg38_Start_Anchor2",
            query_end_col = "hg38_Stop_Anchor2",
            as_granges = TRUE,
            style = "NCBI"
        )
    gr.Corces2020.fitchip <- c(fitchip.anchor1, fitchip.anchor2) 
    #### Merge all together ####
    gr.merged <- unlist(GenomicRanges::GRangesList(
        gr.Nott2019.peaks,
        gr.Nott2019.regions,
        gr.Nott2019.interactome,
        gr.Corces2020.peaks,
        gr.Corces2020.bulk_peaks,
        gr.Corces2020.cicero,
        gr.Corces2020.fitchip
    ))  
    if (!keep_extra_cols) {
        gr.merged <- subset(gr.merged, select = c(Study, Assay, Cell_type))
    } 
    if(!is.null(save_path)){
        messager("Caching gr.merged ==>",save_path,v=verbose)
        dir.create(dirname(save_path),showWarnings = FALSE, recursive = TRUE)
        saveRDS(object = gr.merged,file = save_path)
    }
    return(gr.merged)
}
