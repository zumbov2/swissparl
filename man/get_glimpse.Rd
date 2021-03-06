% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_glimpse.R
\name{get_glimpse}
\alias{get_glimpse}
\title{Retrieve the first rows of a table}
\usage{
get_glimpse(table, rows = 20, Language = "DE")
}
\arguments{
\item{table}{name of the table to glimpse into. For an overview of available tables use \code{\link{get_tables}()}.}

\item{rows}{number of records to download. Maximum is 1000.}

\item{Language}{filter rows by language. Possible are \code{DE}, \code{FR}, \code{IT}, \code{RM}, and \code{EN}.}
}
\value{
A tibble of different length and variable composition.
}
\description{
\code{get_glimpse} retrieves the first rows of a table of the Swiss Parliament WebServices and
    allows a first insight into the data structure.
}
\examples{
\dontrun{
# Short excerpt of table "Person"
get_glimpse(table = "Person")
}
}
