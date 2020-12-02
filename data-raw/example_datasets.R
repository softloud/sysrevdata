library(tidyverse)
library(usethis)


# example data ------------------------------------------------------------


# hazard ------------------------------------------------------------------

hazard <-
  read_csv("data-raw/hazard_example.csv") %>%
  # I think this is a redundant column indicating row number.
  select(-X1)

use_data(hazard, overwrite = TRUE)

# bufferstrips ------------------------------------------------------------

bufferstrips <-
  read_csv("data-raw/Haddawayetal2018bufferstrips.csv")

use_data(bufferstrips, overwrite = TRUE)
