# # Import cell type-specific epigenomic peaks
# #
# # Brain cell-specific epigenomic data from Nott et al. (2019).
# # @keywords internal
# # @family NOTT_2019
# # @source
# # \href{https://science.sciencemag.org/content/366/6469/1134}{Nott et al. (2019)}
# # \url{https://genome.ucsc.edu/cgi-bin/hgTracks?db=hg19&lastVirtModeType=default&lastVirtModeExtraState=&virtModeType=default&virtMode=0&nonVirtPosition=&position=chr2:127770344-127983251&hgsid=778249165_ySowqECRKNxURRn6bafH0yewAiuf}
# # @examples
# # \dontrun{
# # PEAKS.merged <- NOTT_2019.get_epigenomic_peaks(peak.dir="/pd-omics/data/Nott_2019/peaks", narrow_peaks=TRUE, broad_peaks=F)
# # }
# NOTT_2019.get_epigenomic_peaks_macs2 <- function(peak.dir="/pd-omics/data/Nott_2019/peaks",
#                                                  narrow_peaks=TRUE,
#                                                  broad_peaks=TRUE,
#                                                  nThread=1){
#     bigWigFiles <- echoannot::NOTT_2019.bigwig_metadata
#     peak_types <- c(ifelse(narrow_peaks,".narrowPeak$", NA),
#                     ifelse(broad_peaks,"_broad.bed12$", NA))
#     peak_types <- peak_types[!is.na(peak_types)]
#     peaks.paths <- list.files(peak.dir,
#                               pattern =  paste(peak_types, collapse = "|"),
#                               full.names = T,
#                               recursive = T)
#     PEAKS <- MACS2.import_peaks(peaks.paths = peaks.paths,
#                                 as_granges = T)
#     PEAKS <- parallel::mclapply(PEAKS, function(peak){
#         pk.name <- gsub(".ucsc_narrowPeak1|.ucsc_broadRegion1","",peak$name[1])
#         meta <- subset(bigWigFiles, long_name==pk.name)
#         peak$Cell_type <- meta$cell_type
#         peak$Assay <- meta$assay
#         peak$Fresh_frozen <- meta$fresh_frozen
#         peak$Marker  <- meta$marker
#         return(peak)
#     },mc.cores = nThread) %>% GenomicRanges::GRangesList()
#     PEAKS.merged <- unlist(PEAKS)
#     PEAKS.merged$peak_type <- PEAKS.merged
#     return(PEAKS.merged)
# }
#
