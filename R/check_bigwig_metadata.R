check_bigwig_metadata <- function(bigwig_metadata,
                                  required_cols = c("name","data_link")){
    missing_cols <- required_cols[
        !required_cols %in% colnames(bigwig_metadata)
        ]
    if(length(missing_cols)>0){
        stop_msg <- paste("Missing required columns in bigwig_metadata:\n",
                          paste("-",missing_cols,collapse = "\n "))
        stop(stop_msg)
    }
}