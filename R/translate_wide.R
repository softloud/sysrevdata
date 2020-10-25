#' Wide to long
#'
#' @description Convert from wide format to long format based on the prefix for
#' each set of linked values across multiple columns.
#' @param data A dataframe containing rows of systematic review data in wide
#' format.
#' @param column_stem A set of column descriptors signifying the groups of columns to condense
#' based on a common stem (e.g. 'hazard' represents 'hazard_low', 'hazard_moderate', and
#' 'hazard_high').
#' @param value_sep A character used to separate values within the data. The
#' default is set to ';'.
#' @importFrom magrittr '%>%'
#' @return A dataframe in 'long' format.
#' @examples
#' long <- wide_to_long(data, column_stem)
#' long;
#' @export
wide_to_long <- function(data,
                         column_stem,
                         name_sep = '_'){

  for (i in column_stem){
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
#' @param column_stem A set of column descriptors signifying the groups of columns to condense
#' based on a common stem (e.g. 'hazard' represents 'hazard_low', 'hazard_moderate', and
#' 'hazard_high').
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
#' column_stem <- 'hazard'
#' condensed <- wide_to_condensed(data, column_stem, readable = TRUE)
#' condensed;
#' @export
wide_to_condensed <- function(data,
                              column_stem,
                              value_sep = ';',
                              name_sep = '_',
                              readable = TRUE){

  if(readable == TRUE){
    value_sep <- paste(value_sep,
                       ' ',
                       sep = '')
  }

  for (i in column_stem){
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

