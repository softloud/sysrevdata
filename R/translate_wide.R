#' Wide to long
#'
#' @description Convert from wide format to long format based on the prefix for
#' each set of linked values across multiple columns.
#' @param data A dataframe containing rows of systematic review data in wide
#' format.
#' @param columns A set of columns containing coded data from within a dataframe.
#' @param value_sep A character used to separate values within the data. The
#' default is set to ';'.
#' @importFrom magrittr '%>%'
#' @return A dataframe in 'long' format.
#' @examples
#' long <- wide_to_long(data, columns)
#' long;
#' @export
wide_to_long <- function(data,
                         columns,
                         name_sep = '_'){

  for (i in columns){
    data <- data %>%
      tidyr::pivot_longer(cols = starts_with(i),
                   names_prefix = paste(i,
                                        name_sep,
                                        sep = ''),
                   values_to = i,
                   values_drop_na = TRUE) %>%
      dplyr::filter(get(i) != 'NA') %>%
      subset(select = -c(name))
  }
  return(data)
}


#' Wide to condensed
#'
#' @description Convert from wide format to condensed format based on the prefix for
#' each set of linked values across multiple columns.
#' @param data A dataframe containing rows of systematic review data in wide
#' format.
#' @param columns A set of columns containing coded data from within a dataframe.
#' @param value_sep A character used to separate values within the data. The
#' default is set to ';'.
#' @param name_sep A character used to separate terms in the newly generated column
#' names within the data. The default is set to '_' (i.e. snake_case).
#' @param readable Logical argument (TRUE or FALSE) specifying whether the value separator
#' should be followed by a space to improve readability (e.g. '; ' instead of ';').
#' The default is set to 'readable = TRUE'.
#' @importFrom magrittr '%>%'
#' @return A dataframe in 'condensed' format.
#' @examples
#' wide <- wide_to_condensed(data, columns, readable = TRUE)
#' wide;
#' @export
wide_to_condensed <- function(data,
                              columns,
                              value_sep = ';',
                              name_sep = '_',
                              readable = TRUE){

  if(readable == TRUE){
    value_sep <- paste(value_sep,
                       ' ',
                       sep = '')
  }

  for (i in columns){
    data <- data %>%
    tidyr::unite(!!i,
                 starts_with(paste(i,
                                   name_sep,
                                   sep = '')),
                 sep = value_sep,
                 na.rm = TRUE,
                 remove = TRUE)
  }

  return(data)
}
