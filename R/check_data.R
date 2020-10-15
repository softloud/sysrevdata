#' Describe database
#'
#' @description Produce report on dataframe based on possible separators
#' identified within each column.
#' @param data A dataframe containing rows of systematic review data
#' @param value_sep A character used to separate values within the data. The
#' default is set to ';'.
#' @return Returns two outputs: 1) a list of possible candidate separators
#' found within the dataframe; and 2) a summary report describing the
#' database separators.
#' @examples
#' v <- db_summary(data)
#' v$report
#' v$candidate_separators;
#' @export
db_summary <- function(data,
                       value_sep = ';'){

  x <- check_condensed(data, value_sep = value_sep)

  if(any(as.logical(x[,2])) == TRUE){
    r1 <- paste('The following columns in your database contain values separated by [',
                value_sep,
                ']: ',
                paste(subset(x,  candidate_separator == TRUE)[,1],
                      collapse = ', '),
                '.',
                sep = '')
  } else {
    r1 <- paste('No columns were separated by the value provided [',
                value_sep,
                '].',
                sep = '')
  }
  r2 <- paste('See <possible.separators> for a list of potential delimiters that may separate multiple cell values. ',
              'Consider using an alternative separator if coded variables have not been identified.',
              'Use replace.sep() to replace a separator across the dataframe.',
              sep = '')

  report <- paste(r1, r2)
  candidate_separators <- subset(x, select = c(column_name, possible_separators))
  return(list(candidate_separators = candidate_separators, report=report))
}


#' Check dataframe for possible value separators
#'
#' @param data A dataframe containing rows of systematic review data
#' @param value_sep A character used to separate values within the data. The
#' default is set to ';'.
#' @return A dataframe listing all columns in the input data, with a column
#' showing whether the given (or default) value separator was detected in each,
#' and a column showing possible candidate separator (ie. non-alphanumeric)
#' characters found within each column of the input data.
#' @examples
#' check_condensed(data, value_sep = ';');
#' @export
check_condensed <- function(data,
                            value_sep = ';'){
  x <- data.frame(character(), character(), character())
  x <- rbind(x,
             cbind(column_name = names(data),
                   candidate_separator = grepl(value_sep, data),
                   possible_separators = as.vector(sapply(data, function(x)
                     gsub('NA', '', paste(unique(stringr::str_extract(x, '[[:punct:]]|([[:alpha:]])\\1\\1')),
                                          collapse = ' ')))
                   )))
  return(x)
}


#' Check if database is in 'condensed' format
#'
#' @description Checks if separator is present in given coding columns and returns
#' TRUE or FALSE.
#' @param columns A set of columns containing coded data from within a dataframe.
#' @param value_sep A character used to separate values within the data.
#' @return Logical argument, TRUE or FALSE
#' @examples
#' is_condensed(columns, value_sep = ';');
#' @export
is_condensed <- function(columns,
                         value_sep = ';'){
  z <- grep(value_sep, columns)
  if(any(grepl(value_sep, columns) == TRUE)){
    output <- TRUE
    return(output)
  } else {
    output <- FALSE
    return(output)
  }
}


#' Check if database is in 'long' format
#'
#' @description Checks whether key.variable is duplicated AND there are no separated
#' values in coding variables and returns TRUE of FALSE.
#' @param key_variable A variable used to identify unique articles/studies.
#' @param columns A set of columns containing coded data from within a dataframe
#' @param value_sep A character used to separate values within the data.
#' @return Logical argument, TRUE or FALSE
#' @examples
#' is_long(columns, value_sep = ';');
#' @export
is_long <- function(key_variable,
                    columns,
                    value_sep = ';'){
  if(any(grepl(value_sep, columns) == FALSE) & (any(duplicated(key_variable)) == TRUE)){
    output <- TRUE
    return(output)
  } else {
    output <- FALSE
    return(output)
  }
}


#' Check if database is in 'wide' format
#'
#' @description Checks whether key.variable is not duplicated AND there are no
#' separated values in coding variables and returns TRUE of FALSE.
#' @param key_variable A variable used to identify unique articles/studies.
#' @param columns A set of columns containing coded data from within a dataframe
#' @param value_sep A character used to separate values within the data.
#' @return Logical argument, TRUE or FALSE
#' @examples
#' is_wide(key_variable, columns, value_sep = ';');
#' @export
#' is_wide() - c
is_wide <- function(key_variable,
                    columns,
                    value_sep = ';'){
  if(any(grepl(value_sep, columns) == FALSE) & (any(duplicated(key_variable)) == FALSE)){
    output <- TRUE
    return(output)
  } else {
    output <- FALSE
    return(output)
  }
}
