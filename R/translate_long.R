#' Long to wide
#'
#' @description Convert from long format to wide format, with each level of a given
#' column separated into a different column.
#' @param data A dataframe containing rows of systematic review data in long format.
#' @param columns A set of columns containing coded data from within a dataframe.
#' @param name_sep A character used to separate terms in the newly generated column
#' names within the data. The default is set to '_' (i.e. snake_case).
#' @param value_sep A character used to separate values within the data. The
#' default is set to ';'.
#' @importFrom magrittr
#' @return A dataframe in 'wide' format.
#' @examples
#' wide_data2 <- long_to_wide(long_data, columns, value_sep = 'xxx')
#' @export
long_to_wide <- function(data,
                              columns,
                              name_sep = '_',
                              value_sep = ';'){
  for (i in columns){
    data <- data %>%
      tidyr::pivot_wider(names_from = i,
                  values_from = i,
                  values_fn = list,
                  names_prefix = paste(i,
                                       name_sep,
                                       sep = ''),
                  values_fill = list(val = 'NA')) %>%
      dplyr::rename_with(snakecase::to_snake_case) %>%
      replace_string(replace = 'NULL',
                     with = '')
  }
  return(data)
}


#' Long to condensed
#'
#' @description Convert from long format to long format based on the prefix for
#' each set of linked values across multiple columns. Two-step process goes from
#' long to wide and then from wide to condensed.
#' @param data A dataframe containing rows of systematic review data in long
#' format.
#' @param columns A set of columns containing coded data from within a dataframe.
#' @param value_sep A character used to separate values within the data. The
#' default is set to ';'.
#' @param name_sep A character used to separate terms in the newly generated column
#' names within the data. The default is set to '_' (i.e. snake_case).
#' @param readable Logical argument (TRUE or FALSE) specifying whether the value separator
#' should be followed by a space to improve readability (e.g. '; ' instead of ';').
#' The default is set to 'readable = TRUE'.
#' @importFrom magrittr
#' @return A dataframe in 'condensed' format.
#' @examples
#' condensed <- long_to_condensed(data, columns, readable = TRUE)
#' condensed;
#' @export
long_to_condensed <- function(data,
                              columns,
                              value_sep = ';',
                              name_sep = '_',
                              readable = TRUE){

  if(readable == TRUE){
    value_sep <- paste(value_sep,
                       ' ',
                       sep = '')
  }

  #first go long-to-wide
  for (i in columns){
    data <- data %>%
      tidyr::pivot_wider(names_from = i,
                         values_from = i,
                         values_fn = list,
                         names_prefix = paste(i,
                                              name_sep,
                                              sep = ''),
                         values_fill = list(val = 'NA')) %>%
      dplyr::rename_with(snakecase::to_snake_case) %>%
      replace_string(replace = 'NULL',
                     with = '')
  }

  #then go wide-to-condensed
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
