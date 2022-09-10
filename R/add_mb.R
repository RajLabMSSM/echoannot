#' Add Mb
#' 
#' Add column containing genomic coordinate in units of megabasepairs ("Mb").
#' @param dat Data.
#' @param pos_col Genomic position column name.
#' @keywords export
#' @importFrom echodata is_granges
#' @importFrom GenomicRanges mcols
#' @importFrom methods is
#' @importFrom data.table :=
#' @examples 
#' dat <- echodata::BST1
#' dat2 <- add_mb(dat = dat)
add_mb <- function(dat,
                   pos_col = "POS"){
    Mb <- NULL;
    
    if (!"Mb" %in% colnames(dat)) {
        if(echodata::is_granges(dat)){
            GenomicRanges::mcols(dat)["Mb"] <- dat[pos_col] / 1000000
        } else if(methods::is(dat,"data.table")){
            dat[,Mb:=(get(pos_col)/  1000000)]
        } else {
            dat["Mb"] <- dat[pos_col] / 1000000
        }
    }
    return(dat)
}
