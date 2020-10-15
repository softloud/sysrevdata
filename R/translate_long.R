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
      pivot_wider(names_from = i,
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


# now as a function which works specifically with 5 variables...
long_to_condensed <- function(data,
                              key_column,
                              columns,
                              value_sep = ';',
                              readable = TRUE){

  if(readable == TRUE){ #Add space after value separator for easier reading
    value_sep <- paste(value_sep,
                       ' ',
                       sep = '')
  }

  col_names <- names(data)
  extra_cols <- setdiff(col_names, columns)


  t <- long_data %>%
    group_by(article_id) %>%
    summarize(text = stringr::str_c(hazard, collapse = "; "))

collapse.col <- function(data,
                         key_column,
                         value_sep = ';',
                         readable = TRUE){

  if(readable == TRUE){ #Add space after value separator for easier reading
    value_sep <- paste(value_sep,
                       ' ',
                       sep = '')

  for (i in columns){
    tab <- data.table::as.data.table(data)[, toString(i), by = list(key_column)]
    i <- sapply(i, function(x) paste(unique(unlist(stringr::str_split(x, ', '))), collapse = value_sep))
  }
  colnames(t) <- c('article_id', 'hazard')
}

  summarise(long_data, col1 = paste(unique(hazard),
                                    collapse = value_sep))

    summarise(col1 = paste(unique(get(columns[1])),
                           collapse = value.sep),
              col2 = paste(unique(get(columns[2])),
                           collapse = value.sep),
              col3 = paste(unique(get(columns[3])),
                           collapse = value.sep),
              col4 = paste(unique(get(columns[4])),
                           collapse = value.sep),
              col5 = paste(unique(get(columns[5])),
                           collapse = value.sep)
    ) %>%
    rename_all(recode, `<int>` = names(key_column), col1 = columns[1], col2 = columns[2], col3 = columns[3], col4 = columns[4], col5 = columns[5])
  return(x)
}
