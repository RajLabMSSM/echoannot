% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/XGR_download_and_standardize.R
\name{XGR_download_and_standardize}
\alias{XGR_download_and_standardize}
\title{Download, standardize, and merge XGR annotations}
\usage{
XGR_download_and_standardize(
  lib.selections = c("ENCODE_TFBS_ClusteredV3_CellTypes", "TFBS_Conserved",
    "Uniform_TFBS"),
  as_grangesList = FALSE,
  dat = NULL,
  nThread = 1
)
}
\arguments{
\item{lib.selections}{Which XGR annotations to check overlap with.
For full list of libraries see
\href{http://XGR_r-forge.r-project.org/#annotations-at-the-genomic-region-level}{
 here.}
 Passed to the \code{RData.customised} argument in \link[XGR]{xRDataLoader}.}

\item{as_grangesList}{Return as a \code{GRangesList},
instead of a single merged \code{GRanges} object.}

\item{dat}{data.table of genomic coordinates to query with.
Set as \code{NULL} to return genome-wide data.}

\item{nThread}{Number of cores to parallelise across.}
}
\value{
GRangesList
}
\description{
Merges a list of XGR annotations into a single GRanges object
}
\examples{
gr.lib <- echoannot::XGR_download_and_standardize(
    lib.selections = c("ENCODE_DNaseI_ClusteredV3_CellTypes"),
    dat = echodata::BST1
)
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
\code{\link{XGR_iterate_overlap}()},
\code{\link{XGR_merge_and_process}()},
\code{\link{XGR_parse_metadata}()},
\code{\link{XGR_plot_enrichment}()},
\code{\link{XGR_plot_peaks}()},
\code{\link{XGR_prepare_foreground_background}()},
\code{\link{XGR_sep_handler}()},
\code{\link{xgr_query}}
}
\concept{XGR}
