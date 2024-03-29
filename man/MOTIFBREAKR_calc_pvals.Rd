% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MOTIFBREAKR_calc_pvals.R
\name{MOTIFBREAKR_calc_pvals}
\alias{MOTIFBREAKR_calc_pvals}
\title{Calculate \pkg{motifbreakR} p-values}
\usage{
MOTIFBREAKR_calc_pvals(
  mb_res,
  background = c(A = 0.25, C = 0.25, G = 0.25, T = 0.25),
  granularity = NULL,
  nThread = 1,
  results_dir = file.path(tempdir(), "results"),
  verbose = TRUE
)
}
\arguments{
\item{mb_res}{Results generated by \link[echoannot]{MOTIFBREAKR}, in 
\link[GenomicRanges]{GRanges} format.}

\item{background}{Numeric Vector; the background probabilities of the nucleotides}

\item{granularity}{Numeric Vector; the granularity to which to round the PWM,
larger values compromise full accuracy for speed of calculation. A value of
\code{NULL} does no rounding.}

\item{nThread}{Number of threads to parallelize analyses across.}

\item{results_dir}{Directory where results should be saved 
as a file named:
 \emph{<results_dir>/_genome_wide/motifbreakR/motifbreakR_results.rds}.
If \code{NULL}, results will not be saved to disk.}

\item{verbose}{Print messages.}
}
\value{
Motif disruption predictions in 
 \link[GenomicRanges]{GRanges} format, with the p-value columns filled out.
}
\description{
Calculate p-values for each \pkg{motifbreakR} motif disruption result.
}
\examples{
 
mb_res <- MOTIFBREAKR(rsid_list = c("rs11175620"),
                      # limit the number of datasets tests 
                      # for demonstration purposes only
                      pwmList_max = 5)
mb_res_p <- MOTIFBREAKR_calc_pvals(mb_res = mb_res)                       
}
