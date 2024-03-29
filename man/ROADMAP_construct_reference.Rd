% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ROADMAP_construct_reference.R
\name{ROADMAP_construct_reference}
\alias{ROADMAP_construct_reference}
\title{Gather Roadmap annotation metadata}
\usage{
ROADMAP_construct_reference(
  ref_path = system.file("extdata/ROADMAP", "ROADMAP_Epigenomic.js", package =
    "echoannot"),
  limit_files = NULL,
  keyword_query = NULL,
  verbose = TRUE
)
}
\arguments{
\item{ref_path}{Where the ROADMAP metadata is stored.}

\item{limit_files}{Limit the number of annotation files queried
(for faster testing).}

\item{keyword_query}{Search all columns in the Roadmap annotations metadata
and only query annotations that contain your keywords.
Can provide multiple keywords in list form:
\code{c("placenta","liver","monocytes")}.}

\item{verbose}{Print messages.}
}
\description{
Gather Roadmap annotation metadata
}
\examples{
ref <- ROADMAP_construct_reference(keyword_query = c(
    "placenta",
    "liver",
    "monocytes"))
}
\seealso{
Other ROADMAP: 
\code{\link{ROADMAP_merge_and_process}()},
\code{\link{ROADMAP_query}()},
\code{\link{ROADMAP_tabix}()}
}
\concept{ROADMAP}
