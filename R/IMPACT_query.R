#' Query IMPACT annotations
#' 
#' Query annotations/LD-scores generated by
#' \href{https://github.com/immunogenomics/IMPACT}{IMPACT} 
#' (Inference and Modeling of Phenotype-related ACtive Transcription),
#' IMPACT predicts transcription factor (TF) binding at a motif site by 
#' learning the epigenomic profiles at those sites 
#' (primarily \href{https://www.encodeproject.org/}{ENCODE}).
#' All data are aligned to the \emph{hg19} genome build.
#' All data has also been 
#' reformatted to tabix indexed files and uploaded to Zenodo 
#' \href{https://doi.org/10.5281/zenodo.7062238}{here}
#'  to allow for rapid querying.
#' @param types File types to include.
#' @param populations Population ancestries to include 
#' ("EAS" = East Asian; "EUR" = European).
#' @param output_format Output format options:
#' \itemize{
#' \item{"wide" : }{Spread annotation across columns and keep 1 row/SNP.}
#' \item{"long" : }{Melt annotation across rows and allow multiple rows/SNP.}
#' \item{"list" : }{Do not perform merging of queries and instead return
#' results as a named list, where the name is 
#' the file the annotation came from.}
#' }   
#' @param add_metadata Add metadata about each sample 
#' (\emph{Warning}: can substantially increase the dataset size).
#' @inheritParams echotabix::query
#' @inheritParams echotabix::construct_query
#' @returns A named list or data.table of annotations 
#' merged with \code{query_dat}. 
#' 
#' @export
#' @importFrom echotabix construct_query query
#' @importFrom stats setNames
#' @importFrom data.table data.table melt.data.table rbindlist fread :=
#' @importFrom dplyr group_by slice_head
#' @importFrom utils data
#' @examples 
#' query_dat <- echodata::BST1[1:50,]
#' annot_dt <- IMPACT_query(query_dat=query_dat, populations="EUR")
IMPACT_query <- function(query_dat, 
                         types = c("annot","ldscore"),
                         populations = c("EAS","EUR"),
                         query_genome = "hg19",
                         target_genome = "hg19",
                         overlapping_only = TRUE,
                         output_format = c("wide","long","list"),
                         add_metadata = FALSE,
                         conda_env = "echoR_mini",
                         nThread = 1,
                         verbose = TRUE){ 
    
    # echoverseTemplate:::args2vars(echoannot:::IMPACT_query)
    # echoverseTemplate:::source_all()
    IMPACT_id <- IMPACT <- variable <- chrom <- type <- population <- NULL;
    
    IMPACT_files <- get("IMPACT_files")
    #### Handle output_format ####
    output_format <- tolower(output_format)[1]
    if(output_format!="long" && add_metadata){
        msg <- paste(
            "Warning: add_metadata will only work when output_format='long'.",
            "Setting add_metadata=FALSE."
        )
        warning(msg)
    }  
    #### Get file paths ####
    files <- IMPACT_files[chrom %in% unique(query_dat$CHR) &
                             type %in% types &
                             population %in% populations,] 
    query_granges <- echotabix::construct_query(query_dat = query_dat,
                                                verbose = verbose)
    annot <- lapply(stats::setNames(files$url,
                                    files$filename), 
           function(x){  
               echotabix::query(target_path = x,
                                ## Important! Simply appending ".tbi" 
                                ## to end of URL would be incorrect. 
                               target_index =  gsub("\\.bgz",".bgz.tbi",x),
                               query_method = "rsamtools",
                               overlapping_only = overlapping_only,
                               query_granges = query_granges,
                               conda_env = conda_env,
                               nThread = nThread,
                               verbose = verbose)
    })
    #### Process results ####
    id_vars <- c("CHR","BP","SNP")
    #### List mode ####
    if(output_format=="list"){
        return(annot)
    #### Wide mode ####
    } else if (output_format=="wide"){
        annot_dt <- Reduce(function(...){
            merge(..., by=c("query",id_vars))}, annot)
        annot_dt[,-c("CM","query")]
        return(annot_dt)
    #### Long mode ####
    } else if(output_format=="long"){
        annot_dt <- data.table::melt.data.table(
            data = data.table::rbindlist(annot, fill = TRUE, 
                                         use.names = TRUE,
                                         idcol = "filename")[,-c("CM","query")],
            id.vars = c(id_vars,"filename"))
        #### Add metadata ####
        if(isTRUE(add_metadata)){
            messager("Adding metadata to table.",v=verbose)
            meta <- data.table::fread(
                paste("https://github.com/immunogenomics/IMPACT",
                      "raw/master/IMPACT707/IMPACT_annotation_key.txt", 
                      sep="/"))
            #### Tale the first row per annotation ####
            meta <- dplyr::group_by(meta, IMPACT) |> 
                dplyr::slice_head(n = 1) |> 
                data.table::data.table()
            annot_dt[,IMPACT_id:=as.integer(gsub(".*Annot","",variable))]
            annot_dt <- merge(annot_dt, meta, 
                              by.x = "IMPACT_id", 
                              by.y = "IMPACT")
        }
        return(annot_dt)
    } else {
        stp <- paste("output_format must be one of:",
                     paste0("\n - ",eval(formals(IMPACT_query)$output_format),
                            collapse = ""))
        stop(stp)
    }
}
