#' Replace a given separator across dataframe
#'
#' @description Takes a given separator and replaces it with an alternative
#' across the dataframe.
#' @param data A dataframe containing rows of systematic review data.
#' @param old_sep An existing character used to separate values within the
#' data. The default is set to ';'.
#' @param new_sep A replacement character used to separate values within the
#' data. The default is set to ';'.
#' @import magrittr
#' @examples
#' data <- data %>%
#'     replace_sep(old_sep = 'xxx', new_sep = '; ');
#' @export
replace_sep <- function(data,
                        old_sep,
                        new_sep = "; "){
  x <- as.data.frame(lapply(data, gsub, pattern = old_sep, replacement = new_sep))
  return(x)
}


#' Remove whitespace
#'
#' @description Remove 'unnecessary' whitespace (leading spaces and trailing
#' spaces for each cell value and either side of a given separator).
#' @param data A dataframe containing rows of systematic review data.
#' @param value_sep A character used to separate values within the data. The
#' default is set to ';'.
#' @return Dataframe with whitespace removed.
#' @examples
#' data <- trim_spaces(data);
#' @export
trim_spaces <- function(data,
                        value_sep = ';'){
  x <- as.data.frame(mapply(trimws, data))
  z <- paste(value_sep,
             ' | ',
             value_sep,
             sep = '')
  x <- as.data.frame(lapply(data, function(x) gsub(z, value_sep, x)))
}


#' Replace text string across database
#'
#' @description Replace a given string.
#' @param data A dataframe containing rows of systematic review data.
#' @param replace A string to be replaced across the dataframe.
#' @param with A string used to replace the above string.
#' @return A dataframe with the string replaced.
#' @examples
#' data <- replace_string(data, replace = 'NULL', with = '');
replace_string <- function(data,
                           replace,
                           with){
  x <- anchors::replace.value(data,
                              names = colnames(data),
                              from = replace,
                              to = with)
  return(x)
}
