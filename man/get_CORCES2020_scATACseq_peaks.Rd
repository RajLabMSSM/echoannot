% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_CORCES2020_scATACseq_peaks.R
\name{get_CORCES2020_scATACseq_peaks}
\alias{get_CORCES2020_scATACseq_peaks}
\title{scATACseq peaks from human brain tissue}
\source{
\url{https://doi.org/10.1038/s41588-020-00721-x}
}
\usage{
get_CORCES2020_scATACseq_peaks()
}
\description{
Each row represents an individual peak identified in
the single-cell ATAC-seq data.
}
\details{
Data originally from \href{https://doi.org/10.1038/s41588-020-00721-x}{
Corces et al. (bioRxiv)}, as of May 2020.
Specifically: \emph{STable5_Features_scATAC-seq_Peaks_all}
}
\examples{
\dontrun{
dat <- readxl::read_excel(
    file.path(
        "~/Desktop/Fine_Mapping/echolocatoR/annotations",
        "Coceres_2020/STable5_Features_scATAC-seq_Peaks_all.xlsx"
    ),
    skip = 18
)
CORCES2020_scATACseq_peaks <- data.table::data.table(dat)

#### piggyback ####
tmp <- file.path(tempdir(), "CORCES2020_scATACseq_peaks.tsv.gz")
data.table::fwrite(CORCES2020_scATACseq_peaks, tmp, sep = "\t")
piggyback::pb_upload(
    file = tmp,
    repo = "RajLabMSSM/echoannot"
)
}
}
\seealso{
Other CORCES2020: 
\code{\link{CORCES2020_get_ATAC_peak_overlap}()},
\code{\link{CORCES2020_get_hichip_fithichip_overlap}()},
\code{\link{CORCES2020_prepare_bulkATAC_peak_overlap}()},
\code{\link{CORCES2020_prepare_scATAC_peak_overlap}()},
\code{\link{get_CORCES2020_bulkATACseq_peaks}()},
\code{\link{get_CORCES2020_cicero_coaccessibility}()},
\code{\link{get_CORCES2020_hichip_fithichip_loop_calls}()},
\code{\link{get_CORCES2020_scATACseq_celltype_peaks}()}
}
\concept{CORCES2020}
