% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_misc.R
\name{get_affair_types}
\alias{get_affair_types}
\title{Retrieve affair types}
\usage{
get_affair_types(lang = "de", silent = F)
}
\arguments{
\item{lang}{specifies language. Available are German ("de"), French ("fr"), Italian ("it") and English ("en").}

\item{silent}{if \code{TRUE}, no progress bar is displayed.}
}
\value{
A tibble with the following columns:
\itemize{
\item \code{id} ID of affair type.
\item \code{updated} date of last update.
\item \code{abbreviation} language-specific short form of affair type.
\item \code{code} code of affair type.
\item \code{name} full language-specific description of affair type.
}
}
\description{
\code{get_affair_types} retrieves all existing types of affairs (parliamentary instruments etc.) used in the Swiss Parliament.
}
\examples{
\donttest{
get_affair_types(lang = "fr")
}
}
