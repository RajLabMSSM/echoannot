#' Get GEO supplementary files.
#' 
#' Get links to any GEO supplementary files matches one or more of the 
#' \code{regex_queries}.
#' @param gsm GEO GSM id.
#' @param regex_queries Named list of queries.
#' Matches will be case-insensitive.
#' @param verbose Print messages.
#' @keywords internal
#' @importFrom methods new
get_geo_supplementary_files <- function(gsm,
                                        regex_queries = list(
                                            narrowPeaks="narrowpeak",
                                            broadPeaks="broadpeak",
                                            genericPeaks="peak",
                                            bedGraph="bedgraph|graph.gz|bdg.gz",
                                            bigWig="bigwig|bw$"
                                            ),
                                        verbose=TRUE){
    requireNamespace("GEOquery")
    #### Get metadata ####
    g <- GEOquery::getGEO(GEO = gsm) 
    #### Determine file types ####
    supp_urls <- g@header[
        grep("^supplementary_file_*",names(g@header))
    ]
    messager(length(supp_urls),"supplementary file(s) found:",
             paste("\n -",basename(unlist(supp_urls)),collapse = ""),
             v=verbose)
    links <- mapply(regex_queries,FUN=function(x){
        grep(x, unlist(supp_urls),
             ignore.case = TRUE, value = TRUE)
    })
    return(links)
}
