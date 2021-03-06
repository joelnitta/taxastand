% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/etc.R
\name{add_non_darwin_core_cols}
\alias{add_non_darwin_core_cols}
\title{Add non-DarwinCore columns to taxonomic_standard that
are needed for matching.}
\usage{
add_non_darwin_core_cols(taxonomic_standard)
}
\arguments{
\item{taxonomic_standard}{Dataframe of standard names to match to.
Must follow \href{https://dwc.tdwg.org/terms/}{Darwin Core format}.}
}
\value{
Tibble
}
\description{
Adds "genericName", "taxonName", and "speciesName".
}
\details{
"genus" is always the genus of the accepted
name, not the synonym (original name).
But for matching we want the genus of the original name
Some databases (e.g., CoL) provide this as "genericName".
If "genericName" is not already present in the user-provided database,
add it by using the first part of the scientificName.

"speciesName" is 'genus specific_epithet',
e.g. 'Crepidomanes minutum'

"taxonName" is 'genus specific_epithet infraspecific_epithet',
e.g. 'Crepidomanes minutum flabellatum'
}
\examples{
\dontrun{
data(filmy_taxonomy)
add_non_darwin_core_cols(filmy_taxonomy)
}
}
