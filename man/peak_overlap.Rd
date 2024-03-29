% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/peak_overlap.R
\name{peak_overlap}
\alias{peak_overlap}
\title{Get overlap between SNPs and epigenomic peaks}
\usage{
peak_overlap(
  merged_DT,
  snp_filter = "!is.na(SNP)",
  include.NOTT2019_peaks = TRUE,
  include.NOTT2019_enhancers_promoters = TRUE,
  include.NOTT2019_PLACseq = TRUE,
  include.CORCES2020_scATACpeaks = TRUE,
  include.CORCES2020_Cicero_coaccess = TRUE,
  include.CORCES2020_bulkATACpeaks = TRUE,
  include.CORCES2020_HiChIP_FitHiChIP_coaccess = TRUE,
  include.CORCES2020_gene_annotations = TRUE,
  verbose = T
)
}
\description{
Get overlap between SNPs and epigenomic peaks
}
\keyword{internal}
