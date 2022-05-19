split_chromosomes_run <- function(query_granges){
    GenomicRanges::split(
        x = query_granges,
        f = GenomicRanges::seqnames(query_granges))
}