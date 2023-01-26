library(dplyr)

files <- tibble(File = arrange(list.files("MAE"))) %>%
  mutate(Model = grep("Open_to_close"))