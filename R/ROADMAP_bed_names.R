ROADMAP_bed_names <- function(RM_ref,
                              suffix="_15_coreMarks_mnemonics.bed.gz"){
    # All files found here:
    # https://egg2.wustl.edu/roadmap/data/byFileType/chromhmmSegmentations/ChmmModels/coreMarks/jointModel/final/
    
    # Alternative suffixes
    # _15_coreMarks_dense.bed.gz
    # _15_coreMarks_dense.bed.gz
    
    # Create URLs
    bed_names <- lapply(unique(RM_ref$EID), function(eid, 
                                                     suffix.=suffix){
        return(paste0(eid,suffix))
    }) %>% unlist()
    return(bed_names)
}