#' Run \pkg{motifbreakR}
#'
#' \pkg{motifbreakR} is a package to predict how much a SNP will disrupt
#' a transcription factor binding motif (if it falls within one).
#' \emph{Notes:}\cr
#' \itemize{
#' \item{\pkg{BSgenome}}{Users must manually run \code{library(BSgenome)} 
#' before running any \pkg{motifbreakR} functions 
#' to successfully use this tool.
#' }
#' \item{\code{threshold=}}{
#'  If \code{filterp=TRUE}, this argument indicates the p-value threshold.
#'  If \code{filterp=FALSE}, this argument instead indicates the pct threshold.
#'  }
#' }
#' @param rsid_list RSIDs of SNPs to test for motif disruption 
#' between the reference and alternative alleles..
#' @param calculate_pvals Calculate p-values for all SNPs tested.
#' \emph{WARNING:} May take a long time if many SNPs and/or PWM are selected.
#' @param organism Only include datasets in the \code{pwmList} 
#' performed in a particular organism. 
#' @param pwmList_max Limit the maximum number of PWM datasets tested 
#' (e.g. \code{10}). If \code{NULL}, no limit it set.
#' @param results_dir Directory where results should be saved 
#' as a file named:
#'  \emph{<results_dir>/_genome_wide/motifbreakR/motifbreakR_results.rds}.
#' If \code{NULL}, results will not be saved to disk.
#' @param nThread Number of threads to parallelize analyses across.
#' @param force_new If results of the same name already exist, 
#' overwrite them with new analyses (\code{TRUE}). 
#' Otherwise, import the existing results and skip the analyses
#'  (default: \code{FALSE}).
#' @inheritParams select_genome
#' @inheritParams motifbreakR::snps.from.rsid
#' @inheritParams motifbreakR::motifbreakR
#' @inheritParams motifbreakR::calculatePvalue
#' @returns Motif disruption predictions in 
#'  \link[GenomicRanges]{GRanges} format.
#' @family motifbreakR
#' @source
#' \href{https://pubmed.ncbi.nlm.nih.gov/26272984/}{Publication}
#' \href{https://github.com/Simon-Coetzee/MotifBreakR}{GitHub}
#' \href{http://simon-coetzee.github.io/motifBreakR}{Vingette}
#' 
#' @export
#' @examples
#' library(BSgenome) ## <-- IMPORTANT!
#' #### Example fine-mapping results ####
#' merged_DT <- echodata::get_Nalls2019_merged()
#' #### Run motif analyses ####
#' mb_res <- MOTIFBREAKR(rsid_list = c("rs11175620"),
#'                       # limit the number of datasets tested 
#'                       # for demonstration purposes only
#'                       pwmList_max = 4,
#'                       calculate_pvals = FALSE)
MOTIFBREAKR <- function(rsid_list, 
                        results_dir = file.path(tempdir(),"results"),
                        pwmList = NULL,
                        pwmList_max = NULL,
                        genome_build = NULL,
                        organism = "Hsapiens",
                        threshold = .85, # 1e-4 #  4e-8
                        show.neutral = FALSE,
                        method = "default",
                        calculate_pvals = TRUE,
                        force_new = FALSE,
                        background = c(A = 0.25, 
                                       C = 0.25, 
                                       G = 0.25, 
                                       T = 0.25),
                        granularity = NULL,
                        nThread = 1,
                        verbose = TRUE){
  requireNamespace("BSgenome")
  requireNamespace("motifbreakR")
  # echoverseTemplate:::args2vars(MOTIFBREAKR)
    
  #### Select genome build ####
  gb_list <- select_genome(genome_build = genome_build,
                           verbose = verbose)
  #### Set up results save path ####
  rds_path <- file.path(results_dir,'_genome_wide',
                        'motifbreakR',
                        'motifbreakR_results.rds');
  #### Run if file doesn't already exist ####
  if(!file.exists(rds_path) | force_new){
    #### Prepare input SNPs ####
    messager("+ MOTIFBREAKR::",
             "Converting SNP list into motifbreakR input format.",
             v=verbose)
    gr.snps <- motifbreakR::snps.from.rsid(
        rsid = rsid_list,
        dbSNP = gb_list$dbSNP,
        search.genome = gb_list$search.genome);
    #### Subset motif databases ####
    if(is.null(pwmList)){pwmList <- MotifDb::MotifDb}
    if(!is.null(organism)){
      messager("Filtering pwmList to only include organism:",organism)
      pwmList <- pwmList[grep(paste0("^",organism),names(pwmList),
                              value = TRUE,
                              ignore.case = TRUE)]
    } 
    if(!is.null(pwmList_max)){
        messager("Only testing the first",pwmList_max,"datasets in pwmList.",
                 v=verbose)
        pwmList <- pwmList[seq_len(min(length(pwmList),pwmList_max))]
    }
    #### Set up parallelization ####
    # BPPARAM <- get_bpparam(workers = nThread)
    #### Run motifbreakR ####
    messager("+ MOTIFBREAKR:: Identifying motifs and predicting disruptions.", 
             v=verbose) 
    mb_res <- motifbreakR::motifbreakR(snpList = gr.snps,
                                       pwmList = pwmList,
                                       filterp = TRUE,
                                       threshold = threshold,
                                       method = method,
                                       show.neutral = show.neutral,
                                       bkg = background, 
                                       # BPPARAM = BPPARAM,
                                       verbose = verbose);
    #### Exit early if empty ####
    if(length(mb_res)==0) return(mb_res)
    #### Calculate p-values ####
    if(isTRUE(calculate_pvals)){
        mb_res <- MOTIFBREAKR_calc_pvals(mb_res = mb_res, 
                                         results_dir = results_dir,
                                         background = background, 
                                         granularity =  granularity, 
                                         nThread = nThread, 
                                         verbose = verbose)
    } 
    #### Save results #### 
    if(!is.null(results_dir)){
      dir.create(dirname(rds_path),showWarnings = FALSE, recursive = TRUE);
      messager("+ MOTIFBREAKR:: Saving results ==>", rds_path);
      saveRDS(object = mb_res, 
              file = rds_path);
    }
  } else {
    messager( "+ MOTIFBREAKR:: Using pre-existing tmp file.", v=verbose)
    mb_res <- readRDS(rds_path)
  }
  return(mb_res)
}
