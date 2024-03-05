library(data.table)
library(tidyr)
library(stringr)
library(dplyr)
Covs <- c("AttributeLevel", "ScenarioTypeStrict", "ScenarioType",
          "Intervention", "PedPed", "Barrier", "CrossingSignal",
          "NumberOfCharacters", "DiffNumberOFCharacters", "LeftHand",
          "Man", "Woman", "Pregnant", "Stroller", "Dog", "Cat",
          "Boy", "Girl", "Homeless","Criminal",
          "OldMan", "OldWoman", 
          "LargeWoman", "LargeMan", 
          "MaleExecutive", "FemaleExecutive", "FemaleAthlete", "MaleAthlete", 
          "FemaleDoctor", "MaleDoctor"
)

SharedResponses = fread(unzip("MoralMachine/Data/SharedResponsesClean.zip"), 
                        key = "ResponseID", colClasses = "character")
N = nrow(SharedResponses)
n = 10^5
nbatches = ceiling(nrow(SharedResponses) / n)
index = 1:n
Output = tibble()
Cols = colnames(SharedResponses)
from = 1; to = n
for(i in 1:nbatches){
  writeLines(paste0("Batch ",i, " out of ",nbatches,", Rows: ",from," to ",to))
  
  SharedResponsesWide = SharedResponses[index] %>%
    group_by(ResponseID) %>%
    mutate(ResponseID = paste0(ResponseID,"_",row_number())) %>%
    pivot_longer(cols = all_of(c(Covs,"Saved"))) %>% 
    mutate(name = paste0(name,"_",str_extract(ResponseID,"[0-9]+$")),
           ResponseID = str_remove(ResponseID,"_[0-9]+$")) %>% 
    pivot_wider(id_cols = ResponseID, names_from = name,values_from = value) 

  fname = paste0("MoralMachine/Data/Temp/SharedResposesWide_",i,".csv")
  fwrite(SharedResponsesWide, fname)
  Output = bind_rows(Output, SharedResponsesWide)
  
  from = from + n
  to = ifelse(n*(i+1) < N, to + n, to + (N-n*i)) 
  index = from:to

}
cnt = count(Output,ResponseID,sort=T)

fwrite(Output, "MoralMachine/Data/SharedResponsesCleanWide.csv")
zip(zipfile = 'MoralMachine/Data/SharedResponsesCleanWide', files = "MoralMachine/Data/SharedResponsesCleanWide.csv")

# GPT4 predicted responses
GPT4 = fread("MoralMachine/Data/Takemoto2023/data/shared_responses_gpt-4-0613.csv",
             select=c(Covs,"Saved","ResponseID"),
             colClasses="character") %>% 
  pivot_longer(cols = all_of(c(Covs,"Saved"))) %>% 
  mutate(name = paste0(name,"_",str_extract(ResponseID,"[0-9]+$")),
         ResponseID = str_remove(ResponseID,"_[0-9]+$")) %>% 
  pivot_wider(id_cols = ResponseID, names_from = name,values_from = value) 
fwrite(GPT4, "MoralMachine/Data/GPT4wide.csv")
zip(zipfile = 'MoralMachine/Data/GPT4wide', files = "MoralMachine/Data/GPT4wide.csv")


# GPT3 predicted responses
GPT3 = fread("MoralMachine/Data/Takemoto2023/data/shared_responses_gpt-3.5-turbo-0613.csv",
             select=c(Covs,"Saved","ResponseID"),
             colClasses="character") %>% 
  pivot_longer(cols = all_of(c(Covs,"Saved"))) %>% 
  mutate(name = paste0(name,"_",str_extract(ResponseID,"[0-9]+$")),
         ResponseID = str_remove(ResponseID,"_[0-9]+$")) %>% 
  pivot_wider(id_cols = ResponseID, names_from = name,values_from = value) 
fwrite(GPT3, "MoralMachine/Data/GPT3wide.csv")
zip(zipfile = 'MoralMachine/Data/GPT3wide', files = "MoralMachine/Data/GPT3wide.csv")

# Llama 2 predicted responses
Llama2 = fread("MoralMachine/Data/Takemoto2023/data/shared_responses_llama-2-7b-chat.csv",
               select=c(Covs,"Saved","ResponseID"),
               colClasses="character") %>% 
  pivot_longer(cols = all_of(c(Covs,"Saved"))) %>% 
  mutate(name = paste0(name,"_",str_extract(ResponseID,"[0-9]+$")),
         ResponseID = str_remove(ResponseID,"_[0-9]+$")) %>% 
  pivot_wider(id_cols = ResponseID, names_from = name,values_from = value) 
fwrite(Llama2, "MoralMachine/Data/Llama2wide.csv")
zip(zipfile = 'MoralMachine/Data/Llama2wide', files = "MoralMachine/Data/Llama2wide.csv")

# Palm 2 predicted responses
Palm2 = fread("MoralMachine/Data/Takemoto2023/data/shared_responses_palm2.csv",
              select=c(Covs,"Saved","ResponseID"),
              colClasses="character") %>% 
  pivot_longer(cols = all_of(c(Covs,"Saved"))) %>% 
  mutate(name = paste0(name,"_",str_extract(ResponseID,"[0-9]+$")),
         ResponseID = str_remove(ResponseID,"_[0-9]+$")) %>% 
  pivot_wider(id_cols = ResponseID, names_from = name,values_from = value) 
fwrite(Palm2, "MoralMachine/Data/Palm2wide.csv")
zip(zipfile = 'MoralMachine/Data/Palm2wide', files = "MoralMachine/Data/Palm2wide.csv")
