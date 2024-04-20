library(dplyr)
library(readr)
library(stringr)
sr = list.dirs() %>% 
  lapply(function(d) list.files(d, recursive = T)) %>%
  unlist() %>% 
  str_subset("SharedResponses.csv") 

set.seed(03102024)
srs = read_csv(sr[1]) %>%
  filter(UserCountry3 =="USA") %>% 
  group_by(Intervention) %>% 
  sample_n(2500)


CalcTheoreticalInt <- function(X){
  if (X["Intervention"]==0){
    if (X["Barrier"]==0){
      if (X["PedPed"] == 1) p <- 0.48
      else p <- 0.32
      
      if (X["CrossingSignal"]==0) p <- p*0.48
      else if (X["CrossingSignal"]==1) p <- p*0.2
      else p <- p * 0.32
    }
    else p <- 0.2
  }
  else {
    if (X["Barrier"]==0){
      if (X["PedPed"] == 1) {
        p <- 0.48
        if (X["CrossingSignal"]==0) p <- p*0.48
        else if (X["CrossingSignal"]==1) p <- p*0.32
        else p <- p * 0.2
      }
      else {
        p <- 0.2
        if (X["CrossingSignal"]==0) p <- p*0.48
        else if (X["CrossingSignal"]==1) p <- p*0.2
        else p <- p * 0.32
      }
    }
    else p <- 0.32
  }
  return(p)
}

calcWeightsTheoretical <- function(profiles){
  p <- apply(profiles,1,CalcTheoreticalInt)
  return(1/p)
}

srs$weights = calcWeightsTheoretical(srs)

write_csv(srs,"MoralMachine/Data/small_humans.csv")
