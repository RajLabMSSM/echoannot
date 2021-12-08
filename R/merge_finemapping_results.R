#' Merge fine-mapping results from all loci
#'
#' Gather fine-mapping results from \pkg{echolocatoR} across all loci
#' and merge into a single data.frame.
#'
#' @param dataset Path to the folder you want to recursively search
#' for results files within
#'  (e.g. \url{"Data/GWAS/Nalls23andMe_2019"}).
#' Set this to a path that includes multiple subfolders if you want
#' to gather results
#' from multiple studies at once
#' (e.g. \url{"Data/GWAS"}).
#' @param minimum_support Filter SNPs by the minimum number
#'  of fine-mapping tools that contained the SNP in their Credible Set.
#' @param include_leadSNPs Include lead GWAS/QTL SNPs per locus
#'  (regardless of other filtering criterion).
#' @param xlsx_path Save merged data.frame as excel file.
#' @param from_storage Search for stored results files.
#' @param haploreg_annotation Annotate SNPs with HaploReg
#'  (using \code{HaploR}).
#' @param regulomeDB_annotation Annotate SNPs with regulomeDB
#' (using \code{HaploR}).
#' @param biomart_annotation Annotate SNPs with \code{biomart}.
#' @param PP_threshold Mean posterior probability threshold to
#'  include SNPs in mean PP Credible Set
#'  (averaged across all fine-mapping tools).
#' @param consensus_thresh The minimum number of tools that have the SNPs
#' in their Credible Set
#' to classify it as a \strong{Consensus_SNP}.
#' @param exclude_methods Exclude certain fine-mapping methods when estimating
#' \strong{mean.CS} and \strong{Consensus_SNP}.
#' @param verbose Print messages.
#'
#' @export
#' @importFrom dplyr %>%
merge_finemapping_results <- function(dataset = file.path(
                                          tempdir(),
                                          "Data/GWAS"
                                      ),
                                      minimum_support = 1,
                                      include_leadSNPs = TRUE,
                                      LD_reference = NULL,
                                      save_path = tempfile(
                                          fileext = "merged_results.csv.gz"
                                      ),
                                      from_storage = TRUE,
                                      haploreg_annotation = FALSE,
                                      regulomeDB_annotation = FALSE,
                                      biomart_annotation = FALSE,
                                      PP_threshold = .95,
                                      consensus_threshold = 2,
                                      exclude_methods = NULL,
                                      top_CS_only = FALSE,
                                      verbose = TRUE,
                                      nThread = 1) {
    Gene <- Support <- NULL;
    if (from_storage) {
        messager("+ Gathering all fine-mapping results from storage...",
            v = verbose
        )
        # Find all multi-finemap_results files
        multifinemap_pattern <-
            multifinemap_pattern <-
            file.path(dataset, paste0("*.*Multi-finemap.tsv*"))
        multifinemap_pattern2 <-
            multifinemap_pattern <-
            file.path(dataset, paste0("*.*multi_finemap.csv*"))

        multi_dirs <- list.files(
            path = dataset,
            pattern = paste0(c(
                basename(multifinemap_pattern),
                basename(multifinemap_pattern2)
            ), collapse = "|"),
            recursive = T,
            full.names = T
        ) %>% unique()
        if (length(multi_dirs) == 0) {
            stop("+ No multi-finemap files found.")
        } else {
            messager("+", length(multi_dirs), "multi-finemap files found.",
                v = verbose
            )
        }
        loci <- basename(dirname(dirname(multi_dirs)))
        if (length(loci) > length(unique(loci))) {
            messager("+ Removing duplicate Multi-finemap files per locus.",
                v = verbose
            )
            loci <- loci[!duplicated(loci)]
        }
        dataset_names <- dirname(dirname(dirname(multi_dirs))) %>% unique()
        # Loop through each GENE
        finemap_results <- lapply(
            dataset_names,
            function(dn,
                     multi_dirs. = multi_dirs,
                     .nThread = nThread) {
                # Loop through each gene folder
                all_results <- parallel::mclapply(multi_dirs.,
                    function(md,
                             nThread = .nThread) {
                        locus <- basename(dirname(dirname(md)))
                        messager("+ Importing results...", locus, v = verbose)
                        multi_data <- data.table::fread(md, nThread = nThread)
                        multi_data <- echodata::update_cols(multi_data)
                        multi_data <- echodata::assign_lead_snp(
                            dat = multi_data,
                            verbose = verbose
                        )
                        if (!"Locus" %in% colnames(multi_data)) {
                            multi_data <- cbind(Locus = locus, multi_data)
                        }
                        if (!"Dataset" %in% colnames(multi_data)) {
                            multi_data <- cbind(
                                Dataset = basename(dn),
                                multi_data
                            )
                        }
                        return(multi_data)
                        # Bind loci
                    },
                    mc.cores = nThread
                ) %>%
                    data.table::rbindlist(fill = TRUE)
            }
        ) %>% data.table::rbindlist(fill = TRUE) # Bind datasets
    }
    #### Add/Update Support/Consensus cols ####
    merged_results <- echodata::find_consensus_snps(
        dat = finemap_results,
        credset_thresh = PP_threshold,
        consensus_thresh = consensus_threshold,
        exclude_methods = exclude_methods,
        verbose = verbose
    )
    merged_results <- subset(merged_results, Support >= minimum_support)
    if (!"Locus" %in% colnames(merged_results)) {
        merged_results <- merged_results %>%
            dplyr::rename(Locus = Gene) %>%
            data.table::data.table()
    }

    #### Annotate with haplorR ####
    if (haploreg_annotation) {
        HR_query <- haplor_haploreg(
            snp_list = unique(merged_results$SNP),
            verbose = verbose
        )
        merged_results <- data.table::merge.data.table(
            x = data.table::data.table(merged_results),
            y = data.table::data.table(HR_query),
            by.x = "SNP",
            by.y = "rsID",
            all = TRUE,
            allow.cartesian = TRUE
        )
    }
    if (regulomeDB_annotation) {
        regDB_query <- haplor_regulomedb(
            snp_list = unique(merged_results$SNP),
            verbose = verbose
        )
        merged_results <- data.table::merge.data.table( 
            x = data.table::data.table(merged_results),
            y = data.table::data.table(regDB_query),
            by.x = "SNP",
            by.y = "rsID",
            all = TRUE,
            allow.cartesian = TRUE
        )
    }
    #### Annotate with biomart ####
    if (biomart_annotation) {
        biomart_query <- biomart_snp_info(
            snp_list = merged_results$SNP,
            verbose = verbose
        )
        merged_results <- data.table::merge.data.table( 
            x = data.table::data.table(merged_results),
            y = data.table::data.table(biomart_query),
            by.x = "SNP",
            by.y = "refsnp_id",
            all = TRUE,
            allow.cartesian = TRUE
        )
    }
    #### Save ####
    if (save_path != FALSE) {
        dir.create(dirname(save_path), showWarnings = FALSE, recursive = TRUE)
        # openxlsx::write.xlsx(merged_results, xlsx_path)
        messager("+ Saving merged results ==>", save_path, v = verbose)
        data.table::fwrite(merged_results, save_path, nThread = nThread)
    }
    return(merged_results)
}
