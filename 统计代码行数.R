library(dplyr)
list.files(pattern = "\\.(R|r|js|css)$",
             full.names = TRUE,
             recursive = TRUE) %>%
  sapply(function(file) {
    readLines() %>% length()
  }) %>%
  sum()