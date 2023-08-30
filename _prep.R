library(tidyverse)
map(dir("_data"), function(file) {
  read_csv(here::here("_data", file)) %>%
    names() %>%
    tibble(variable = .) %>%
    mutate(table = str_replace(file, ".csv", ""))
}) %>%
  do.call(rbind, .) %>%
  select(table, variable) %>%
  arrange(table, variable) %>%
  mutate(include_as = NA,
         filter_of = NA) %>%
  write_csv(here::here("_data", "meta.csv"), na = "")
