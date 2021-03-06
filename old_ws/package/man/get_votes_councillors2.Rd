% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_votes.R
\name{get_votes_councillors2}
\alias{get_votes_councillors2}
\title{Retrieve all individual vote decisions}
\usage{
get_votes_councillors2(lang = "de", silent = F)
}
\arguments{
\item{lang}{specifies language. Available are German ("de"), French ("fr"), Italian ("it") and English ("en").}

\item{silent}{if \code{TRUE}, no progress bar is displayed.}
}
\value{
A tibble with 16 columns, including \code{councillorVote.decision}, \code{meaningYes}, \code{meaningNo} and \code{divisionText}.
}
\description{
\code{get_votes_councillors2} is wrapper around \code{get_votes_councillors} and enables the mass retrieval of
    individual vote decisions of members of the National Council provided by the WebServices of the Swiss Parliament.
}
\examples{
\donttest{
get_votes_councillors2()
}
}
