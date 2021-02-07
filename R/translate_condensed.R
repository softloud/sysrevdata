#' Condensed to long
#'
#' @description Convert from condensed format to long format based on the presence
#' of a given value separator within a given set of columns.
#' @param data A dataframe containing rows of systematic review data in condensed
#' format.
#' @param columns A set of columns containing coded data from within a dataframe.
#' @param value_sep A character used to separate values within the data. The
#' default is set to ';'.
#' @importFrom magrittr '%>%'
#' @return A dataframe in 'long' format.
#' @export
condensed_to_long <- function(data,
                              columns,
                              value_sep = ';'){
  for (i in columns){
    data <- data %>%
      tidyr::separate_rows(.,
                           i,
                           sep = value_sep)
  }
  return(data)
}


#' Condensed to wide
#'
#' @description Convert from condensed format to wide format based on the presence
#' of a given value separator within a given set of columns.
#' @param data A dataframe containing rows of systematic review data in condensed
#' format.
#' @param columns A set of columns containing coded data from within a dataframe.
#' @param name_sep A character used to separate terms in the newly generated column
#' names within the data. The default is set to '_' (i.e. snake_case).
#' @param value_sep A character used to separate values within the data. The
#' default is set to ';'.
#' @importFrom magrittr '%>%'
#' @return A dataframe in 'wide' format.
#' @export
condensed_to_wide <- function(data,
                              columns,
                              name_sep = '_',
                              value_sep = ';'){
  for (i in columns){
    data <- data %>%
      tidyr::separate_rows(.,
                           i,
                           sep = value_sep) %>%
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
