# import the data (from sysrev sankey in Rfunctions)
data <- read.csv('data.csv')
data <- read.csv(file.choose())#, row.names = 1

library(tidyr)
library(dplyr)


#' Part 1
#' Tidying and understanding the database
#'
#' Create dataframe showing whether multiple values exist for each column - not working for separator identification
check.condensed <- function(data, value.sep = ';'){
  x <- data.frame(character(), character(), character())
  x <- rbind(x,
             cbind(column.name = names(data),
                   candidate.separator = grepl(value.sep, data),
                   possible.separators = as.vector(sapply(data, function(x)
                     gsub("NA","",paste(unique(stringr::str_extract(x, '[[:punct:]]|([[:alpha:]])\\1\\1')), collapse = ' ')))
                   ))
  )
  return(x)
}
check.condensed(data, value.sep = 'xxx')


#' Replace a given separator across dataframe
replace.sep <- function(data, old.sep, new.sep = "; "){
  x <- as.data.frame(lapply(data, gsub, pattern = old.sep, replacement = new.sep))
  return(x)
}
data <- data %>%
  replace.sep(old.sep = 'xxx', new.sep = '; ')


#' Remove 'unnecessary' whitespace (leading spaces and trailing spaces for each cell value and either side of a given separator)
trim_spaces <- function(data, value.sep = ';'){
  x <- as.data.frame(mapply(trimws, data))
  z <- paste(value.sep, ' | ', value.sep, sep = '')
  x <- as.data.frame(lapply(data, function(x) gsub(z, value.sep, x)))
}
m <- trim_spaces(sample)

#' Produce report on dataframe
db.summary <- function(data, sep = ';'){
  x <- check.condensed(data, sep = sep)
  if(any(as.logical(x[,2])) == TRUE){
    r1 <- paste('The following columns in your database contain values separated by [',
                sep,
                ']: ',
                paste(subset(x,  candidate.separator == TRUE)[,1], collapse = ', '),
                '.', sep = ''
    )
  } else {
    r1 <- paste('No columns were separated by the value provided [',
                sep,
                '].', sep = '')
  }
  r2 <- paste('See <possible.separators> for a list of potential delimiters that may separate multiple cell values. ',
              'Consider using an alternative separator if coded variables have not been identified.',
              'Use replace.sep() to replace a separator across the dataframe.',
              sep = ''
  )
  report <- paste(r1, r2)
  candidate.separators <- subset(x, select = c(column.name, possible.separators))
  return(list(candidate.separators = candidate.separators, report=report))
}

v <- db.summary(data)
v$report
v$candidate.separators


#' Function to identify the type of database based on the presence of separators and the input of the key variable (e.g. study ID)
#' Input must be a dataframe or table

#' is.condensed() - checks if separator is present in given coding columns and returns TRUE or FALSE
is.condensed <- function(columns, value.sep = ';'){
  z <- grep(value.sep, columns)
  if(any(grepl(value.sep, columns) == TRUE)){
    output <- TRUE
    return(output)
  } else {
    output <- FALSE
    return(output)
  }
}

columns <- as_tibble(cbind(data$region, data$hazard, data$response, data$sdgs, data$actor))
is.condensed(columns, value.sep = 'xxx')

#' is.long() - checks whether key.variable is duplicated AND there are no separated values in coding variables and returns TRUE of FALSE
is.long <- function(key.variable, columns, value.sep = ';'){
  if(any(grepl(value.sep, columns) == FALSE) & (any(duplicated(key.variable)) == TRUE)){
    output <- TRUE
    return(output)
  } else {
    output <- FALSE
    return(output)
  }
}

columns <- as_tibble(cbind(long$region, long$hazard, long$response, long$sdgs, long$actor))
# Do not run this as an example, takes too long
is.long(key.variable = long$article_id, columns = columns, value.sep = 'xxx')

#' is.wide() - checks whether key.variable is not duplicated AND there are no separated values in coding variables and returns TRUE of FALSE
is.wide <- function(key.variable, columns, value.sep = ';'){
  if(any(grepl(value.sep, columns) == FALSE) & (any(duplicated(key.variable)) == FALSE)){
    output <- TRUE
    return(output)
  } else {
    output <- FALSE
    return(output)
  }
}

columns <- cbind(wide[,4:54])
is.wide(key.variable = wide$article_id, columns = columns, value.sep = 'xxx') #currently doesn't work on this example because one line is duplicated in data.csv



#' Part 2 - translating the database formats
#'
#' a) go from condensed to long (works)
# pipeline code
sep <- 'xxx'
long <- data %>%
  separate_rows(region, sep = sep) %>%
  separate_rows(hazard, sep = sep) %>%
  separate_rows(response, sep = sep) %>%
  separate_rows(sdgs, sep = sep) %>%
  separate_rows(actor, sep = sep)

#' now as a function that accepts up to 10 columns
condensed_to_long <- function(data, columns, sep = '; '){
  l <- length(columns)
  x <- data %>%
    separate_rows(columns[1], sep = sep)
  if (l == 1){
    return(x)
  } else {
    x <- x %>%
      separate_rows(columns[2], sep = sep)
    if (l == 2){
      return(x)
    } else {
      x <- x %>%
        separate_rows(columns[3], sep = sep)
      if (l == 3){
        return (x)
      } else {
        x <- x %>%
          separate_rows(columns[4], sep = sep)
        if (l == 4){
          return(x)
        } else {
          x <- x %>%
            separate_rows(columns[5], sep = sep)
          if (l == 5) {
            return(x)
          } else {
            x <- x %>%
              separate_rows(columns[6], sep = sep)
            if (l == 6){
              return (x)
            } else {
              x <- x %>%
                separate_rows(columns[7], sep = sep)
              if (l == 7){
                return(x)
              } else {
                x <- x %>%
                  separate_rows(columns[8], sep = sep)
                if (l == 8){
                  return(x)
                } else {
                  x <- x %>%
                    separate_rows(columns[9], sep = sep)
                  if (l == 9){
                    return(x)
                  } else {
                    x <- x %>%
                      separate_rows(columns[10], sep = sep)
                    if (l == 10){
                      return(x)
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

columns <- c('region', 'hazard', 'response', 'sdgs', 'actor')
long <- condensed_to_long(data, columns, sep = 'xxx')


#' b) go from long to wide and convert columns names to snake_case (works) - values_fill is not working so wrote a bespoke function to call

# function replaces 'NULL' with NA
replace.null <- function(data){
  x <- anchors::replace.value(data, names = colnames(data), from = 'NULL', to = '')
  return(x)
}

# pipeline code
wide <- long %>%
  pivot_wider(names_from = hazard,
              values_from = hazard,
              values_fn = list,
              names_prefix = 'hazard_',
              values_fill = list(val = '')) %>%
  dplyr::rename_with(snakecase::to_snake_case) %>%
  replace.null()

# now as a function
long_to_wide <- function(data, variables, name.sep = '_'){
  l <- length(variables)
  vnames <- paste(variables,
                  name.sep,
                  sep = '')
  x <- data %>%
    pivot_wider(names_from = variables[1],
                values_from = variables[1],
                values_fn = list,
                names_prefix = vnames[1],
                values_fill = list(val = ''))
  if (l == 1){
    x <- x %>%
      dplyr::rename_with(snakecase::to_snake_case) %>%
      replace.null()
    return(x)
  } else {
    x <- x %>%
      pivot_wider(names_from = variables[2],
                  values_from = variables[2],
                  values_fn = list,
                  names_prefix = vnames[2],
                  values_fill = list(val = ''))
    if (l == 2){
      x <- x %>%
        dplyr::rename_with(snakecase::to_snake_case) %>%
        replace.null()
      return(x)
    } else {
      x <- x %>%
        pivot_wider(names_from = variables[3],
                    values_from = variables[3],
                    values_fn = list,
                    names_prefix = vnames[3],
                    values_fill = list(val = ''))
      if (l == 3){
        x <- x %>%
          dplyr::rename_with(snakecase::to_snake_case) %>%
          replace.null()
        return (x)
      } else {
        x <- x %>%
          pivot_wider(names_from = variables[4],
                      values_from = variables[4],
                      values_fn = list,
                      names_prefix = vnames[4],
                      values_fill = list(val = ''))
        if (l == 4){
          x <- x %>%
            dplyr::rename_with(snakecase::to_snake_case) %>%
            replace.null()
          return(x)
        } else {
          x <- x %>%
            pivot_wider(names_from = variables[5],
                        values_from = variables[5],
                        values_fn = list,
                        names_prefix = vnames[5],
                        values_fill = list(val = ''))
          if (l == 5) {
            x <- x %>%
              dplyr::rename_with(snakecase::to_snake_case) %>%
              replace.null()
            return(x)
          } else {
            x <- x %>%
              pivot_wider(names_from = variables[6],
                          values_from = variables[6],
                          values_fn = list,
                          names_prefix = vnames[6],
                          values_fill = list(val = ''))
            if (l == 6){
              x <- x %>%
                dplyr::rename_with(snakecase::to_snake_case) %>%
                replace.null()
              return (x)
            } else {
              x <- x %>%
                pivot_wider(names_from = variables[7],
                            values_from = variables[7],
                            values_fn = list,
                            names_prefix = vnames[7],
                            values_fill = list(val = ''))
              if (l == 7){
                x <- x %>%
                  dplyr::rename_with(snakecase::to_snake_case) %>%
                  replace.null()
                return(x)
              } else {
                x <- x %>%
                  pivot_wider(names_from = variables[8],
                              values_from = variables[8],
                              values_fn = list,
                              names_prefix = vnames[8],
                              values_fill = list(val = ''))
                if (l == 8){
                  x <- x %>%
                    dplyr::rename_with(snakecase::to_snake_case) %>%
                    replace.null()
                  return(x)
                } else {
                  x <- x %>%
                    pivot_wider(names_from = variables[9],
                                values_from = variables[9],
                                values_fn = list,
                                names_prefix = vnames[9],
                                values_fill = list(val = ''))
                  if (l == 9){
                    x <- x %>%
                      dplyr::rename_with(snakecase::to_snake_case) %>%
                      replace.null()
                    return(x)
                  } else {
                    x <- x %>%
                      pivot_wider(names_from = variables[10],
                                  values_from = variables[10],
                                  values_fn = list,
                                  names_prefix = vnames[10],
                                  values_fill = list(val = ''))
                    if (l == 10){
                      x <- x %>%
                        dplyr::rename_with(snakecase::to_snake_case) %>%
                        replace.null()
                      return(x)
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

variables <- columns
wide <- long_to_wide(long, variables)


# go from wide to long (!!!works but loses one row somewhere!!!)
long2 <- wide %>%
  pivot_longer(cols = starts_with("hazard"),
               names_prefix = "hazard_",
               values_to = "hazard",
               values_drop_na = TRUE) %>%
  select(-name) %>%
  filter(!is.na(hazard))

# now as a function
wide_to_long <- function(data, variables, name.sep = '_'){
  l <- length(variables)
  vnames <- paste(variables,
                  name.sep,
                  sep = '')
  x <- data %>%
    pivot_longer(cols = starts_with(variables[1]),
                 names_prefix = vnames[1],
                 values_to = variables[1],
                 values_drop_na = TRUE) %>%
    filter(get(variables[1]) != 'NA') %>%
    subset(select = -c(name))
  if (l == 1){
    return(x)
  } else {
    x <- x %>%
      pivot_longer(cols = starts_with(variables[2]),
                   names_prefix = vnames[2],
                   values_to = variables[2],
                   values_drop_na = TRUE) %>%
      filter(get(variables[2]) != 'NA') %>%
      subset(select = -c(name))
    if (l == 2){
      return(x)
    } else {
      x <- x %>%
        pivot_longer(cols = starts_with(variables[3]),
                     names_prefix = vnames[3],
                     values_to = variables[3],
                     values_drop_na = TRUE) %>%
        filter(get(variables[3]) != 'NA') %>%
        subset(select = -c(name))
      if (l == 3){
        return (x)
      } else {
        x <- x %>%
          pivot_longer(cols = starts_with(variables[4]),
                       names_prefix = vnames[4],
                       values_to = variables[4],
                       values_drop_na = TRUE) %>%
          filter(get(variables[4]) != 'NA') %>%
          subset(select = -c(name))
        if (l == 4){
          return(x)
        } else {
          x <- x %>%
            pivot_longer(cols = starts_with(variables[5]),
                         names_prefix = vnames[5],
                         values_to = variables[5],
                         values_drop_na = TRUE) %>%
            filter(get(variables[5]) != 'NA') %>%
            subset(select = -c(name))
          if (l == 5) {
            return(x)
          } else {
            x <- x %>%
              pivot_longer(cols = starts_with(variables[6]),
                           names_prefix = vnames[6],
                           values_to = variables[6],
                           values_drop_na = TRUE) %>%
              filter(get(variables[6]) != 'NA') %>%
              subset(select = -c(name))
            if (l == 6){
              return (x)
            } else {
              x <- x %>%
                pivot_longer(cols = starts_with(variables[7]),
                             names_prefix = vnames[7],
                             values_to = variables[7],
                             values_drop_na = TRUE) %>%
                filter(get(variables[7]) != 'NA') %>%
                subset(select = -c(name))
              if (l == 7){
                return(x)
              } else {
                x <- x %>%
                  pivot_longer(cols = starts_with(variables[8]),
                               names_prefix = vnames[8],
                               values_to = variables[8],
                               values_drop_na = TRUE) %>%
                  filter(get(variables[8]) != 'NA') %>%
                  subset(select = -c(name))
                if (l == 8){
                  return(x)
                } else {
                  x <- x %>%
                    pivot_longer(cols = starts_with(variables[9]),
                                 names_prefix = vnames[9],
                                 values_to = variables[9],
                                 values_drop_na = TRUE) %>%
                    filter(get(variables[9]) != 'NA') %>%
                    subset(select = -c(name))
                  if (l == 9){
                    return(x)
                  } else {
                    x <- x %>%
                      pivot_longer(cols = starts_with(variables[10]),
                                   names_prefix = vnames[10],
                                   values_to = variables[10],
                                   values_drop_na = TRUE) %>%
                      filter(get(variables[10]) != 'NA') %>%
                      subset(select = -c(name))
                    if (l == 10){
                      return(x)
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

long2 <- wide_to_long(wide, variables)

# go from condensed to wide (works)
wide <- data %>%
  separate_rows(hazard, sep = 'xxx') %>%
  pivot_wider(names_from = hazard, values_from = hazard, values_fn = list, names_prefix = "hazard_", values_fill = list(val = 'NA')) %>%
  dplyr::rename_with(snakecase::to_snake_case)
wide <- anchors::replace.value(wide, names = colnames(wide), from = 'NULL', to = '')

#now in a function
condensed_to_wide <- function(data, variables, name.sep = '_', value.sep = '; '){
  l <- length(variables)
  vnames <- paste(variables,
                  name.sep,
                  sep = '')
  x <- data %>%
    separate_rows(variables[1],
                  sep = value.sep) %>%
    pivot_wider(names_from = variables[1],
                values_from = variables[1],
                values_fn = list,
                names_prefix = vnames[1],
                values_fill = list(val = 'NA')) %>%
    dplyr::rename_with(snakecase::to_snake_case) %>%
    replace.null(.)
  if (l == 1){
    return(x)
  } else {
    x <- x %>%
      separate_rows(variables[2],
                    sep = value.sep) %>%
      pivot_wider(names_from = variables[2],
                  values_from = variables[2],
                  values_fn = list,
                  names_prefix = vnames[2],
                  values_fill = list(val = 'NA')) %>%
      dplyr::rename_with(snakecase::to_snake_case) %>%
      replace.null(.)
    if (l == 2){
      return(x)
    } else {
      x <- x %>%
        separate_rows(variables[3],
                      sep = value.sep) %>%
        pivot_wider(names_from = variables[3],
                    values_from = variables[3],
                    values_fn = list,
                    names_prefix = vnames[3],
                    values_fill = list(val = 'NA')) %>%
        dplyr::rename_with(snakecase::to_snake_case) %>%
        replace.null(.)
      if (l == 3){
        return(x)
      } else {
        x <- x %>%
          separate_rows(variables[4],
                        sep = value.sep) %>%
          pivot_wider(names_from = variables[4],
                      values_from = variables[4],
                      values_fn = list,
                      names_prefix = vnames[4],
                      values_fill = list(val = 'NA')) %>%
          dplyr::rename_with(snakecase::to_snake_case) %>%
          replace.null(.)
        if (l == 4){
          return(x)
        } else {
          x <- x %>%
            separate_rows(variables[5],
                          sep = value.sep) %>%
            pivot_wider(names_from = variables[5],
                        values_from = variables[5],
                        values_fn = list,
                        names_prefix = vnames[5],
                        values_fill = list(val = 'NA')) %>%
            dplyr::rename_with(snakecase::to_snake_case) %>%
            replace.null(.)
          if (l == 5){
            return(x)
          } else {
            x <- x %>%
              separate_rows(variables[6],
                            sep = value.sep) %>%
              pivot_wider(names_from = variables[6],
                          values_from = variables[6],
                          values_fn = list,
                          names_prefix = vnames[6],
                          values_fill = list(val = 'NA')) %>%
              dplyr::rename_with(snakecase::to_snake_case) %>%
              replace.null(.)
            if (l == 6){
              return(x)
            } else {
              x <- x %>%
                separate_rows(variables[7],
                              sep = value.sep) %>%
                pivot_wider(names_from = variables[7],
                            values_from = variables[7],
                            values_fn = list,
                            names_prefix = vnames[7],
                            values_fill = list(val = 'NA')) %>%
                dplyr::rename_with(snakecase::to_snake_case) %>%
                replace.null(.)
              if (l == 7){
                return(x)
              } else {
                x <- x %>%
                  separate_rows(variables[8],
                                sep = value.sep) %>%
                  pivot_wider(names_from = variables[8],
                              values_from = variables[8],
                              values_fn = list,
                              names_prefix = vnames[8],
                              values_fill = list(val = 'NA')) %>%
                  dplyr::rename_with(snakecase::to_snake_case) %>%
                  replace.null(.)
                if (l == 8){
                  return(x)
                } else {
                  x <- x %>%
                    separate_rows(variables[9],
                                  sep = value.sep) %>%
                    pivot_wider(names_from = variables[9],
                                values_from = variables[9],
                                values_fn = list,
                                names_prefix = vnames[9],
                                values_fill = list(val = 'NA')) %>%
                    dplyr::rename_with(snakecase::to_snake_case) %>%
                    replace.null(.)
                  if (l == 9){
                    return(x)
                  } else {
                    x <- x %>%
                      separate_rows(variables[2],
                                    sep = value.sep) %>%
                      pivot_wider(names_from = variables[2],
                                  values_from = variables[2],
                                  values_fn = list,
                                  names_prefix = vnames[2],
                                  values_fill = list(val = 'NA')) %>%
                      dplyr::rename_with(snakecase::to_snake_case) %>%
                      replace.null(.)
                    if (l == 10){
                      return(x)
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

b <- condensed_to_wide(data, variables, name.sep = '_', value.sep = 'xxx')


# need to go from wide to condensed (works)
condensed <- wider %>%
  unite(hazard,
        starts_with('hazard_'),
        sep = '; ',
        na.rm = TRUE)

#now in a function
wide_to_condensed <- function(data, variables, value.sep = ';', nice = TRUE){
  l <- length(variables)
  vnames <- paste(variables,
                  name.sep,
                  sep = '')
  if(nice == TRUE){
    value.sep <- paste(value.sep, ' ', sep = '')
  }
  x <- data %>%
    unite(variables[1],
          starts_with(vname[1]),
          sep = value.sep,
          na.rm = TRUE)
  if (l == 1){
    return(x)
  } else {
    x <- x %>%
      unite(variables[1],
            starts_with(vname[1]),
            sep = value.sep,
            na.rm = TRUE)
    if (l == 2){
      return(x)
    } else {

      if (l == 3){
        return(x)
      } else {

        if (l == 4){
          return(x)
        } else {

          if (l == 5){
            return(x)
          } else {

            if (l == 6){
              return(x)
            } else {

              if (l == 7){
                return(x)
              } else {

                if (l == 8){
                  return(x)
                } else {

                  if (l == 9){
                    return(x)
                  } else {

                    if (l == 10){
                      return(x)
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

# go from long to condensed
condensed <- long %>%
  group_by(article_id) %>%
  summarise(col1 = paste(unique(get(variables[1])),
                         collapse = "; "),
            col2 = paste(unique(get(variables[2])),
                         collapse = "; "),
            col3 = paste(unique(get(variables[3])),
                         collapse = "; "),
            col4 = paste(unique(get(variables[4])),
                         collapse = "; "),
            col5 = paste(unique(get(variables[5])),
                         collapse = "; ")
  ) %>%
  rename_all(recode, col1 = variables[1], col2 = variables[2], col3 = variables[3], col4 = variables[4], col5 = variables[5])

# now as a function which works specifically with 5 variables...
long_to_condensed <- function(data, key_column, columns, value.sep = ';', nice = TRUE){
  if(nice == TRUE){
    value.sep <- paste(value.sep, ' ', sep = '')
  }
  col_names <- names(data)
  variables <- columns
  extra_cols <- setdiff(col_names, variables)
  x <- data %>%
    group_by({{key_column}}) %>%
    summarise(col1 = paste(unique(get(variables[1])),
                           collapse = value.sep),
              col2 = paste(unique(get(variables[2])),
                           collapse = value.sep),
              col3 = paste(unique(get(variables[3])),
                           collapse = value.sep),
              col4 = paste(unique(get(variables[4])),
                           collapse = value.sep),
              col5 = paste(unique(get(variables[5])),
                           collapse = value.sep)
    ) %>%
    rename_all(recode, `<int>` = names(key_column), col1 = variables[1], col2 = variables[2], col3 = variables[3], col4 = variables[4], col5 = variables[5])
  return(x)
}

m <- long_to_condensed(long, key_column = article_id, columns = variables)

t <- "col1 = paste(unique(get(variables[1])), collapse = '; '),col2 = paste(unique(get(variables[2])),collapse = '; '),col3 = paste(unique(get(variables[3])),collapse = '; '),col4 = paste(unique(get(variables[4])), collapse = '; '),col5 = paste(unique(get(variables[5])), collapse = '; ')"
te <- data %>%
  group_by({{key_column}}) %>%
  eval(parse(text=paste('te <- data %>% group_by({{key_column}}) %>% summarise(',t,')',sep='')))

t <- data.frame(c(12, 13))
k <- c('13', '16')
com <- 'cbind(t, k)'
t <- t %>%
  eval(parse(text = com))

# replace '|||' with 'xxx'
#data <- data.frame(lapply(data, function(x) {
#  gsub('\\|\\|\\|', "xxx", x)
#  gsub('xxx ', "xxx", x)
#}))
