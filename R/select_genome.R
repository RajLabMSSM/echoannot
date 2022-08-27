#' Select genome build
#' 
#' Select and load a valid genome build and its relevant  Bioc data packages.
#' @keywords internal
#' @param genome_build Genome build to use.
#' @returns Named list
select_genome <- function(genome_build){
    if(is.null(genome_build)){
        messager("genome_build set to hg19 by default.")
        genome_build <- "hg19"
    }
    genome_build <- tolower(genome_build)[1]
    if(genome_build=="hg19"){
        requireNamespace("SNPlocs.Hsapiens.dbSNP144.GRCh37")
        requireNamespace("BSgenome.Hsapiens.UCSC.hg19")
        dbSNP <- SNPlocs.Hsapiens.dbSNP144.GRCh37::SNPlocs.Hsapiens.dbSNP144.GRCh37
        search.genome <- BSgenome.Hsapiens.UCSC.hg19::BSgenome.Hsapiens.UCSC.hg19
    } else if(genome_build=="hg38"){
        requireNamespace("SNPlocs.Hsapiens.dbSNP144.GRCh38")
        requireNamespace("BSgenome.Hsapiens.UCSC.hg38")
        dbSNP <- SNPlocs.Hsapiens.dbSNP144.GRCh38::SNPlocs.Hsapiens.dbSNP144.GRCh38
        search.genome <- BSgenome.Hsapiens.UCSC.hg38::BSgenome.Hsapiens.UCSC.hg38
    } else{
        stp <- paste("genome_build must be one of:\n",
                     paste0("- ",c("hg19","hg38"),sep="\n ",collapse = ""))
        stop(stp)
    }
    return(list(genome_build=genome_build,
                dbSNP=dbSNP,
                search.genome=search.genome))
}
