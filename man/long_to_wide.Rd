% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/translate_long.R
\name{long_to_wide}
\alias{long_to_wide}
\title{Long to wide}
\usage{
long_to_wide(data, columns, name_sep = "_", value_sep = ";")
}
\arguments{
\item{data}{A dataframe containing rows of systematic review data in long format.}

\item{columns}{A set of columns containing coded data from within a dataframe.}

\item{name_sep}{A character used to separate terms in the newly generated column
names within the data. The default is set to '_' (i.e. snake_case).}

\item{value_sep}{A character used to separate values within the data. The
default is set to ';'.}
}
\value{
A dataframe in 'wide' format.
}
\description{
Convert from long format to wide format, with each level of a given
column separated into a different column.
}
\examples{
\dontrun{
wide_data2 <- long_to_wide(long_data, columns, value_sep = 'xxx')
}
}
