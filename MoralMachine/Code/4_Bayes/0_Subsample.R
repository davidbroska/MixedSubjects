library(dplyr)
library(readr)
library(stringr)
sr = list.dirs() %>% 
  lapply(function(d) list.files(d, recursive = T)) %>%
  unlist() %>% 
  str_subset("SharedResponses.csv") 

srs = read_csv(sr[1]) %>%
  filter(UserCountry3 =="USA") %>% 
  group_by(Intervention) %>% 
  sample_n(2500)

write_csv(srs,"MoralMachine/Data/small_humans.csv")