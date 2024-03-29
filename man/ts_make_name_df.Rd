% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{ts_make_name_df}
\alias{ts_make_name_df}
\title{Make a dataframe with taxonomic names}
\usage{
ts_make_name_df(taxa)
}
\arguments{
\item{taxa}{Character vector; taxon names to be parsed by taxon-tools \code{parsenames}.
Missing values not allowed. Must all be unique.}
}
\value{
Dataframe with two columns: \code{id} and \code{name}
}
\description{
Make a dataframe with taxonomic names
}
\examples{
\dontrun{
ts_make_name_df("Foogenus x barspecies var. foosubsp (L.) F. Bar")
}
}
\keyword{internal}
