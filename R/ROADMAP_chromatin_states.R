ROADMAP_chromatin_states <- function(){
    # Term key for chromatin states
    path <- system.file("extdata","ROADMAP","ROADMAP_chromatinState_HMM.tsv",
                        package = "echoannot")
    data.table::fread(path) 
}
