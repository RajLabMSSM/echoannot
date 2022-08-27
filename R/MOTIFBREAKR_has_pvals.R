MOTIFBREAKR_has_pvals <- function(mb_res){
    if(methods::is(mb_res,"GRanges")){
        dat <- GenomicRanges::mcols(mb_res)[,c("Refpvalue","Altpvalue")]
    } else if (methods::is(mb_res,"data.frame")){
        dat <- mb_res[,c("Refpvalue","Altpvalue")]
    } else {
        stop("mb_res must be of class GRanges or data.frame.")
    }
    has_pvals <- !all(is.na(dat))
    return(has_pvals)
}