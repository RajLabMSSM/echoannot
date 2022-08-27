#' Calculate \pkg{motifbreakR} p-values
#' 
#' Calculate p-values for each \pkg{motifbreakR} motif disruption result.
#' 
#' @inheritParams MOTIFBREAKR
#' @inheritParams MOTIFBREAKR_filter
#' @inheritParams motifbreakR::calculatePvalue
#' @returns Motif disruption predictions in 
#'  \link[GenomicRanges]{GRanges} format, with the p-value columns filled out.
#'  
#' @export
#' @importFrom BiocGenerics %in%
#' @examples  
#' mb_res <- MOTIFBREAKR(rsid_list = c("rs11175620"),
#'                       # limit the number of datasets tests 
#'                       # for demonstration purposes only
#'                       pwmList_max = 5)
#' mb_res_p <- MOTIFBREAKR_calc_pvals(mb_res = mb_res)                       
MOTIFBREAKR_calc_pvals <- function(mb_res, 
                                   background = c(A = 0.25, 
                                                  C = 0.25, 
                                                  G = 0.25, 
                                                  T = 0.25),
                                   granularity = NULL,
                                   nThread = 1,
                                   results_dir=file.path(tempdir(),"results"),
                                   verbose=TRUE){
    requireNamespace("motifbreakR");
    requireNamespace("MotifDb");
    requireNamespace("BSgenome");
    #### Calculate p-values #### 
    messager("+ MOTIFBREAKR:: Calculating p-values for",
             length(mb_res),"RSID(s)...", v=verbose)
    # BPPARAM <- get_bpparam(workers = nThread)
    mb_res_p <- motifbreakR::calculatePvalue(results = mb_res,
                                             # BPPARAM = BPPARAM,
                                             background = background, 
                                             granularity = granularity) 
    #### Save results ####
    if(!is.null(results_dir)){
        rds_path <- file.path(results_dir,
                              '_genome_wide',
                              'motifbreakR',
                              'motifbreakR_results.rds');
        messager("+ MOTIFBREAKR:: Saving updated results ==>",rds_path,
                 v=verbose)
        dir.create(dirname(rds_path), showWarnings = FALSE, recursive = TRUE)
        saveRDS(mb_res_p, rds_path);
    }
    return(mb_res_p)
}
