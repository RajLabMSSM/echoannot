#' Annotate merged fine-mapping results from all loci
#'
#' Annotate fine-mapping results from \pkg{echolocatoR} across all loci.
#' @param dat Table containing at least a SNP column.
#' @param SNP_col Name of the column in \code{dat} 
#' that contain's each SNPs RSID.
#' @param haploreg_annotation Annotate SNPs with HaploReg
#'  (using \code{HaploR}).
#' @param regulomeDB_annotation Annotate SNPs with regulomeDB
#' (using \code{HaploR}).
#' @param biomart_annotation Annotate SNPs with \code{biomart}.
#' @param verbose Print messages.
#' 
#' @export
#' @importFrom dplyr rename 
#' @importFrom data.table merge.data.table data.table rbindlist 
#' @examples
#' dat <- echodata::BST1[Consensus_SNP==TRUE,]
#' dat_annot <- annotate_snps(dat = dat)
annotate_snps <- function(dat,
                          SNP_col = "SNP",
                          haploreg_annotation = TRUE,
                          regulomeDB_annotation = TRUE,
                          biomart_annotation = TRUE, 
                          verbose = TRUE) {
    
    dat <- data.table::data.table(dat)
    if(!SNP_col %in% colnames(dat)){
        stp <- paste("SNP_col must be present in dat.")
        stop(stp)
    }
    #### Annotate with haplorR ####
    if (isTRUE(haploreg_annotation)) {
        HR_query <- haplor_haploreg(
            snp_list = unique(dat[[SNP_col]]),
            verbose = verbose
        )
        dat <- data.table::merge.data.table(
            x = dat,
            y = HR_query,
            by.x = SNP_col,
            by.y = "rsID",
            all = TRUE,
            allow.cartesian = TRUE
        )
    }
    if (isTRUE(regulomeDB_annotation)) {
        regDB_query <- haplor_regulomedb(
            snp_list = unique(dat[[SNP_col]]),
            verbose = verbose
        )
        dat <- data.table::merge.data.table( 
            x = dat,
            y = regDB_query,
            by.x = SNP_col,
            by.y = "rsID",
            all = TRUE,
            allow.cartesian = TRUE
        )
    }
    #### Annotate with biomart ####
    if (isTRUE(biomart_annotation)) {
        biomart_query <- biomart_snp_info(
            snp_list = dat[[SNP_col]],
            verbose = verbose
        )
        dat <- data.table::merge.data.table( 
            x = dat,
            y = biomart_query,
            by.x = SNP_col,
            by.y = "refsnp_id",
            all = TRUE,
            allow.cartesian = TRUE
        )
    } 
    return(dat)
}
