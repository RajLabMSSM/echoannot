is_bigwig <- function(path){
    grepl("bigwig$|bw$",path,ignore.case = TRUE)
}