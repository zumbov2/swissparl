% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_affairsummaries.R
\name{get_affairsummaries2}
\alias{get_affairsummaries2}
\title{Retrieve all affair summaries}
\usage{
get_affairsummaries2(year = NULL, lang = "de", silent = F)
}
\arguments{
\item{year}{numeric or character vector of years to query. If \code{is.null(year)} all available summaries are retrieved.}

\item{lang}{specifies language. Available are German ("de"), French ("fr"), Italian ("it") and English ("en").}

\item{silent}{if \code{TRUE}, no progress bar is displayed.}
}
\value{
A tibble with 7 columns, including \code{description}, \code{initialSituation} and \code{proceedings}.
}
\description{
\code{get_affairsummaries2} is wrapper around \code{get_affairsummaries} and enables the mass retrieval of
    summaries of political affairs provided by the WebServices of the Swiss Parliament.
}
\examples{
\donttest{
get_affairsummaries2(year = c(1999:2003))
}
}
