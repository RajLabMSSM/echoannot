% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/XGR_iterate_overlap.R
\name{XGR_iterate_overlap}
\alias{XGR_iterate_overlap}
\title{Check overlap with XGR annotations}
\usage{
XGR_iterate_overlap(
  lib.selections = c("ENCODE_TFBS_ClusteredV3_CellTypes", "TFBS_Conserved",
    "ReMap_PublicAndEncode_TFBS", "Uniform_TFBS"),
  dat,
  save_path = FALSE,
  nThread = 1
)
}
\arguments{
\item{lib.selections}{Which XGR annotations to check overlap with.
For full list of libraries see
\href{http://XGR_r-forge.r-project.org/#annotations-at-the-genomic-region-level}{
 here.}
 Passed to the \code{RData.customised} argument in \link[XGR]{xRDataLoader}.}

\item{dat}{Data.frame with at least the following columns:
\describe{
\item{SNP}{SNP RSID}
\item{CHR}{chromosome}
\item{POS}{position}
}}

\item{save_path}{Save the results as a \code{data.frame}.}

\item{nThread}{Number of threads to parallelise across libraries.}
}
\description{
Automatically handles different file formats provided by XGR
 (e.g. varying kinds of nested/unnested \code{GRanges}).
Then returns a \code{Granges} object with only the XGR annotation ranges
that overlap with the SNPs in \code{dat}.
The \code{GRanges} merges hits from \code{dat}.
}
\examples{
\dontrun{
gr.hits <- XGR_iterate_overlap(
    lib.selections = c("ENCODE_TFBS_ClusteredV3_CellTypes"),
    dat = echodata::BST1
)
}
}
\seealso{
Other XGR: 
\code{\link{XGR_enrichment_bootstrap}()},
\code{\link{XGR_enrichment_plot}()},
\code{\link{XGR_enrichment}()},
\code{\link{XGR_filter_assays}()},
\code{\link{XGR_filter_sources}()},
\code{\link{XGR_import_annotations}()},
\code{\link{XGR_iterate_enrichment}()},
\code{\link{XGR_merge_and_process}()},
\code{\link{XGR_parse_metadata}()},
\code{\link{XGR_plot_enrichment}()},
\code{\link{XGR_prepare_foreground_background}()},
\code{\link{XGR_query}()},
\code{\link{XGR_sep_handler}()},
\code{\link{xgr_example}}
}
\concept{XGR}
\keyword{internal}
