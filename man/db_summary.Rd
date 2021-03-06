% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_data.R
\name{db_summary}
\alias{db_summary}
\title{Describe database}
\usage{
db_summary(data, value_sep = ";")
}
\arguments{
\item{data}{A dataframe containing rows of systematic review data.}

\item{value_sep}{A character used to separate values within the data. The
default is set to ';'.}
}
\value{
Returns two outputs: 1) a list of possible candidate separators
found within the dataframe; and 2) a summary report describing the
database separators.
}
\description{
Produce report on dataframe based on possible separators
identified within each column.
}
\examples{
\dontrun{
v <- db_summary(data)
v$report
v$candidate_separators
}
}
