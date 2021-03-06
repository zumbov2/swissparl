% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_members.R
\name{get_committee_members}
\alias{get_committee_members}
\title{Retrieve committee memberships}
\usage{
get_committee_members(committee_id, lang = "de", silent = F)
}
\arguments{
\item{committee_id}{numeric or character vector of committee IDs to query.}

\item{lang}{specifies language. Available are German ("de"), French ("fr"), Italian ("it") and English ("en").}

\item{silent}{if \code{TRUE}, no progress bar is displayed.}
}
\value{
A tibble with 24 columns, including \code{faction}, \code{gender}, \code{language} and \code{committeeFunctionName}.
}
\description{
\code{get_committee_members} retrieves data on committee memberships provided by the WebServices of the Swiss Parliament.
}
\examples{
\donttest{
get_committee_members(committee_id = c(1:3))
}
}
