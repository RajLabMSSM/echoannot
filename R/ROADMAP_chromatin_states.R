ROADMAP_chromatin_states <- function(as_dict=FALSE){
    # Term key for chromatin states
    path <- system.file("extdata","ROADMAP","ROADMAP_chromatinState_HMM.tsv",
                        package = "echoannot")
    d <- data.table::fread(path) 
    if(isTRUE(as_dict)){
        ckey <- stats::setNames(d$DESCRIPTION, d$MNEMONIC)
        #### Inferring missing descriptions ####
        ckey["Enh"] <- 'Enhancer'
        ckey["EnhG"] <- 'Genic enhancer'
        ckey["TssAFlnk"] <- 'Active flanking TSS'
        ckey["BivFlnk"] <- 'Bivalent/Poised flanking TSS' 
        return(ckey)
    } else {
        return(d)
    }
}
