## ----setup, include=TRUE------------------------------------------------------
library(echoannot)

## ----eval=FALSE---------------------------------------------------------------
# merged_DT <- echodata::get_Nalls2019_merged()

## ----eval=FALSE---------------------------------------------------------------
# #### Only query high-confidence fine-mapping SNPs from one locus ####
# dat <- merged_DT[Locus=="LRRK2" & Consensus_SNP==TRUE,]
# #### Query annotations ####
# dat_annot <- echoannot::annotate_snps(dat = dat,
#                                       haploreg_annotation = TRUE,
#                                       regulomeDB_annotation = TRUE,
#                                       biomart_annotation = TRUE)
# knitr::kable(dat_annot)

## ----eval=FALSE---------------------------------------------------------------
# gg_cs_bin <- echoannot::CS_bin_plot(merged_DT = merged_DT,
#                                     show_plot = FALSE)

## ----eval=FALSE---------------------------------------------------------------
# gg_cs_counts <- echoannot::CS_counts_plot(merged_DT = merged_DT,
#                                           show_plot = FALSE)

## ----eval=FALSE---------------------------------------------------------------
# gg_epi <- echoannot::peak_overlap_plot(
#     merged_DT = merged_DT,
#     include.NOTT2019_enhancers_promoters = TRUE,
#     include.NOTT2019_PLACseq = TRUE,
#     #### Omit many annotations to save time ####
#     include.NOTT2019_peaks = FALSE,
#     include.CORCES2020_scATACpeaks = FALSE,
#     include.CORCES2020_Cicero_coaccess = FALSE,
#     include.CORCES2020_bulkATACpeaks = FALSE,
#     include.CORCES2020_HiChIP_FitHiChIP_coaccess = FALSE,
#     include.CORCES2020_gene_annotations = FALSE)

## ----eval=FALSE---------------------------------------------------------------
# super_plot <- echoannot::super_summary_plot(merged_DT = merged_DT,
#                                             plot_missense = FALSE)
# 

## ----Cleanup, include=FALSE, eval=FALSE---------------------------------------
# remove(super_plot, gg_epi, gg_cs_counts, merged_DT)

## ----Session Info-------------------------------------------------------------
utils::sessionInfo()

