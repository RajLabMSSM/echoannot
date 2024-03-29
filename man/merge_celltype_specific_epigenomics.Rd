% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/merge_celltype_specific_epigenomics.R
\name{merge_celltype_specific_epigenomics}
\alias{merge_celltype_specific_epigenomics}
\title{Merge all cell-type-specific epigenomics}
\usage{
merge_celltype_specific_epigenomics(
  keep_extra_cols = FALSE,
  save_path = file.path(tools::R_user_dir(package = "echoannot", which = "cache"),
    "merge_celltype_specific_epigenomics.rds"),
  force_new = FALSE,
  verbose = TRUE
)
}
\arguments{
\item{keep_extra_cols}{Keep extra columns
that are not shared across all annotations.}

\item{save_path}{Path to save merged results to.}

\item{force_new}{If cached merged results already exist, ignore them
and recreate the file anyway.}

\item{verbose}{Print messages.}
}
\description{
Merges multiple cell-type-specific epigenomic datasets
(Nott 2019, Corces 2020) into a single \link[GenomicRanges]{GRanges} object.
}
\examples{
gr.merged <- echoannot::merge_celltype_specific_epigenomics()
}
