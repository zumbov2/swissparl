% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_votes.R
\name{get_votes_affairs}
\alias{get_votes_affairs}
\title{Retrieve votes on specific affairs}
\usage{
get_votes_affairs(affair_id, lang = "de", silent = F)
}
\arguments{
\item{affair_id}{numeric or character vector of affair IDs to query. See \code{get_affairs} for IDs.}

\item{lang}{specifies language. Available are German ("de"), French ("fr"), Italian ("it") and English ("en").}

\item{silent}{if \code{TRUE}, no progress bar is displayed.}
}
\value{
A tibble with 17 columns, including the voting results.
}
\description{
\code{get_votes_affairs} retrieves affair specific vote data of the National Council provided by the
    WebServices of the Swiss Parliament.
}
\examples{
\donttest{
# All votes on USRIII
get_votes_affairs(affair_id = 20150049)
}
}
