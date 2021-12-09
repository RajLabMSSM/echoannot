add_mb <- function(dat,
                   pos_col = "POS"){
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