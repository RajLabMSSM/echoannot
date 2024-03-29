% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/annotate_snps.R
\name{annotate_snps}
\alias{annotate_snps}
\title{Annotate merged fine-mapping results from all loci}
\usage{
annotate_snps(
  dat,
  SNP_col = "SNP",
  haploreg_annotation = TRUE,
  regulomeDB_annotation = TRUE,
  biomart_annotation = TRUE,
  verbose = TRUE
)
}
\arguments{
\item{dat}{Table containing at least a SNP column.}

\item{SNP_col}{Name of the column in \code{dat} 
that contain's each SNPs RSID.}

\item{haploreg_annotation}{Annotate SNPs with HaploReg
(using \code{HaploR}).}

\item{regulomeDB_annotation}{Annotate SNPs with regulomeDB
(using \code{HaploR}).}

\item{biomart_annotation}{Annotate SNPs with \code{biomart}.}

\item{verbose}{Print messages.}
}
\description{
Annotate fine-mapping results from \pkg{echolocatoR} across all loci.
}
\examples{
dat <- echodata::BST1[Consensus_SNP==TRUE,]
dat_annot <- annotate_snps(dat = dat)
}
