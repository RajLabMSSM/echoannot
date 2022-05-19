get_genome <- function(build){
    requireNamespace("regioneR")
    regioneR::filterChromosomes(
        A = regioneR::getGenome(build),
        keep.chr = paste0("chr",c(seq_len(22),"X","Y")) 
    ) 
}