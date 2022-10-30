#' Test enrichment
#' 
#' Conduct permutation enrichment tests between all combinations of 
#' two named lists,
#' each containing one or more \link[GenomicRanges]{GRanges} objects.
#' Permutation tests are run using \link[regioneR]{overlapPermTest}. 
#' 
#' @param grlist1 First list of \link[GenomicRanges]{GRanges} objects.
#' @param grlist2 Second list of \link[GenomicRanges]{GRanges} objects.
#' @param genome The reference genome to use. A valid genome object. 
#' Either a GenomicRanges or data.frame containing one region per whole 
#' chromosome or a character uniquely identifying a genome in 
#' BSgenome (e.g. "hg19", "mm10" but not "hg"). 
#' Internally it uses getGenomeAndMask.
#' @param seed Set the seed for reproducibility.
#' @param mc.set.seed 
#' "In order to create reproducible code with functions that use random 
#' numbers such as the permutation testing in \pkg{regioneR} , 
#' it is necessary to use set.seed. 
#' However, since \pkg{regioneR} uses parallel to perform the test it is
#'  also necessary to set the \code{mc.set.seed} parameter to FALSE 
#'  to ensure reproducibility."
#' @param verbose Print messages.
#' @param ... Additional arguments passed to \link[regioneR]{overlapPermTest}.
#' @inheritParams regioneR::overlapPermTest
#' @inheritParams regioneR::permTest 
#' @source \href{https://www.bioconductor.org/packages/devel/bioc/vignettes/regioneR/inst/doc/regioneR.html}{
#' See section "3.7A note on reproducibility" for info on setting the seed.}
#' @returns data.frame
#' 
#' @export
#' @importFrom data.table data.table rbindlist 
#' @examples
#' dat <- echodata::get_Nalls2019_merged() 
#' grlist1 <- dat[P<5e-8,]
#' grlist2 <- dat[Support>0,] 
#' enrich <- test_enrichment(grlist1 = grlist1,
#'                           grlist2 = grlist2,  
#'                           ntimes = 25) 
test_enrichment <- function(grlist1,
                            grlist2, 
                            ntimes = 100,
                            genome = "hg19", 
                            alternative = "auto", 
                            min.parallel=1000,
                            force.parallel=NULL,
                            seed = 2022,
                            mc.set.seed = FALSE,
                            verbose = TRUE,
                            ...){
    
    requireNamespace("regioneR")
    #### Check inputs #### 
    grlist1 <- check_grlist(grlist=grlist1, 
                            prefix = "grlist1",
                            verbose = verbose)
    grlist2 <- check_grlist(grlist=grlist2,
                            prefix = "grlist2",
                            verbose = verbose)
    
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
                                            min.parallel=min.parallel,
                                            force.parallel=force.parallel,
                                            mc.set.seed=mc.set.seed,
                                            verbose=verbose,
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
        }) |> data.table::rbindlist()
        return(res2)
    }) |> data.table::rbindlist()
    #### Return ####
    return(enrich)
}
