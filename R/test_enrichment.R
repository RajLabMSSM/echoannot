#' Test enrichment
#' 
#' Conduct permutation enrichment tests between all combinations of 
#' two named lists,
#' each containing one or more \link[GenomicRanges]{GRanges} objects.
#' Permutation tests are run using \link[regioneR]{overlapPermTest}. 
#' 
#' @param grlist1 First list of \link[GenomicRanges]{GRanges} objects.
#' @param grlist2 Second list of \link[GenomicRanges]{GRanges} objects.
#' @param ... Additional arguments passed to \link[regioneR]{overlapPermTest}.
#' @inheritParams regioneR::overlapPermTest
#' 
#' @export
#' @importFrom dplyr %>%
#' @importFrom data.table data.table rbindlist 
test_enrichment <- function(grlist1,
                            grlist2, 
                            ntimes = 50,
                            genome = "hg19", 
                            alternative = "auto",
                            verbose = TRUE,
                            ...){
    requireNamespace("regioneR")
    #### Check inputs #### 
    grlist1 <- check_grlist(grlist=grlist1)
    grlist2 <- check_grlist(grlist=grlist2)
    
    enrich <- lapply(names(grlist1), function(nm1){
        if(verbose) cat("\ngrlist1:",nm1,"\n")
        res2 <- lapply(names(grlist2)[!is.na(grlist2)], function(nm2){
            messager("+ grlist2: ",nm2, v=verbose)    
            t1 <- Sys.time()
            A <- grlist1[[nm1]]
            B <- grlist2[[nm2]]
            pt <- regioneR::overlapPermTest(A=A, 
                                            B=B,
                                            ntimes=ntimes, 
                                            genome=genome,
                                            alternative=alternative,
                                            ...)
            res1 <- data.table::data.table(
                gr1=nm1,
                gr2=nm2,
                gr1_n=length(A),
                gr2_n=length(B),
                numOverlaps=regioneR::numOverlaps(A=A, B=B, 
                                                  count.once = TRUE),
                meanDistance=regioneR::meanDistance(A=A, B=B),
                observed=pt$numOverlaps$observed,
                pval=pt$numOverlaps$pval,
                ntimes=pt$numOverlaps$ntimes,
                alternative=pt$numOverlaps$alternative, 
                # permuted=list(pt$numOverlaps$permuted),
                zscore=pt$numOverlaps$zscore,
                full_res=pt
            )
            t2 <- Sys.time()
            res1$time <- as.numeric(t2-t1) 
            return(res1)
        }) %>% data.table::rbindlist()
        return(res2)
    }) %>% data.table::rbindlist()
    #### Return ####
    return(enrich)
}
