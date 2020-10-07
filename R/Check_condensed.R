#' Check dataframe for multiple possible separators
#' @param data A data frame
#' @param value.sep An expected column separator
#' @examples
#' data(bufferstrips)
#' check_condensed(bufferstrips, value.sep = ';')
#' @export
#'
check_condensed <- function(data, value.sep = ';'){
  x <- data.frame(character(), character(), character())
  x <- rbind(x,
             cbind(column.name = names(data),
                   candidate.separator = grepl(value.sep, data),
                   possible.separators = as.vector(sapply(data, function(x)
                     gsub("NA","",paste(unique(stringr::str_extract(x, '[[:punct:]]|([[:alpha:]])\\1\\1')), collapse = ' ')))
                   )))
  return(x)
}

#check_condensed(bufferstrips, value.sep = ';')
