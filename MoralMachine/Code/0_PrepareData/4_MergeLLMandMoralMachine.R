library(tidyr)
library(dplyr)
library(stringr)
library(data.table)

# Load human responses
SharedResponsesWide = fread(unzip("MoralMachine/Data/SharedResponsesCleanWide.zip"), 
                            key = "ResponseID")

# Confirm that ID is unique
all(table(SharedResponsesWide$ResponseID) == 1)

# No NA values
all(sapply(SharedResponsesWide, function(x) sum(is.na(x))) == 0)

# Define columns to join on
Cols = colnames(SharedResponsesWide)
JoinVars = Cols[- which(Cols %in% c("ResponseID","Saved_1","Saved_2"))]
UniqVars = c(JoinVars,"Intervention_1","Intervention_2")
# Keep unique rows
SRWD = distinct(SharedResponsesWide, pick(all_of(JoinVars)), .keep_all=T)

mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Load LLM data
GPT4 = fread(unzip("MoralMachine/Data/GPT4wide.zip")) %>% 
  group_by(across(all_of(JoinVars))) %>% 
  mutate(#SavedProp_1 = mean(Saved_1), SavedProp_2 = mean(Saved_2),
         #Saved_1 = mode(Saved_1),  Saved_2 = mode(Saved_2)
         ) %>% 
  ungroup() %>% 
  distinct(pick(all_of(JoinVars)), .keep_all=T)
  
GPT3 = fread(unzip("MoralMachine/Data/GPT3wide.zip")) %>% 
  group_by(across(all_of(JoinVars))) %>% 
  mutate(#SavedProp_1 = mean(Saved_1), SavedProp_2 = mean(Saved_2),
         #Saved_1 = mode(Saved_1), Saved_2 = mode(Saved_2)
         ) %>% 
  ungroup() %>% 
  distinct(pick(all_of(JoinVars)), .keep_all=T)

Llama2 = fread(unzip("MoralMachine/Data/Llama2wide.zip")) %>% 
  group_by(across(all_of(JoinVars))) %>% 
  mutate(#SavedProp_1 = mean(Saved_1), SavedProp_2 = mean(Saved_2),
         #Saved_1 = mode(Saved_1), Saved_2 = mode(Saved_2)
         ) %>% 
  ungroup() %>% 
  distinct(pick(all_of(JoinVars)), .keep_all=T)

Palm2 = fread(unzip("MoralMachine/Data/Palm2wide.zip")) %>% 
  group_by(across(all_of(JoinVars))) %>% 
  mutate(#SavedProp_1 = mean(Saved_1), SavedProp_2 = mean(Saved_2),
         #Saved_1 = mode(Saved_1), Saved_2 = mode(Saved_2)
         ) %>% 
  ungroup() %>% 
  distinct(pick(all_of(JoinVars)), .keep_all=T)

# Confirm that IDs in LLM responses are unique
all(table(GPT4$ResponseID)==1)
all(table(GPT3$ResponseID)==1)
all(table(Llama2$ResponseID)==1)
all(table(Palm2$ResponseID)==1)



JoinedWide = SRWD %>% 
  left_join(GPT4, by=JoinVars,suffix = c("","GPT4"))  %>% 
  left_join(GPT3, by=JoinVars,suffix = c("","GPT3"))  %>% 
  left_join(Llama2, by=JoinVars,suffix = c("","Llama2"))  %>% 
  left_join(Palm2, by=JoinVars,suffix = c("","Palm2")) %>% 
  filter(!is.na(Saved_1GPT4)|!is.na(Saved_1GPT3)|!is.na(Saved_1Llama2)|!is.na(Saved_1Palm2))



lm(scale(Saved_1GPT4) ~ scale(Saved_1),JoinedWide)
lm(scale(Saved_1GPT3) ~ scale(Saved_1),JoinedWide)
lm(scale(Saved_1Llama2) ~ scale(Saved_1),JoinedWide)
lm(scale(Saved_1Palm2) ~ scale(Saved_1),JoinedWide)

# To long data format
d1 = JoinedWide %>% 
  select(ResponseID,matches("_1"),matches("ResponseID[A-Za-z]+[0-9]")) %>% 
  rename_all(~ str_remove(., "_[1-2]"))
d2 = JoinedWide %>%
  select(ResponseID,matches("_2"),matches("ResponseID[A-Za-z]+[0-9]")) %>% 
  rename_all(~ str_remove(., "_[1-2]"))
JoinedLong = bind_rows(d1,d2)


JoinedWide %>% 
  select(ResponseID,matches("Saved_1")) %>% 
  pivot_longer(cols = matches("GPT3|GPT4|Llama2|Palm2")) %>% 
  group_by(name) %>% 
  summarise(NumberMatch = sum(!is.na(value)), 
            Correlation = cor(Saved_1,value,use="complete.obs"))
JoinedLong %>% 
  select(ResponseID,matches("Saved")) %>% 
  pivot_longer(cols = matches("GPT3|GPT4|Llama2|Palm2")) %>% 
  group_by(name) %>% 
  summarise(NumberMatch = sum(!is.na(value)), 
            Correlation = cor(Saved,value,use="complete.obs"))
count(JoinedLong,ResponseID,sort = T)

# save to file
fwrite(JoinedWide, "MoralMachine/Data/JoinedWide.csv")
zip(zipfile = 'MoralMachine/Data/JoinedWide', files = "MoralMachine/Data/JoinedWide.csv")
fwrite(JoinedLong, "MoralMachine/Data/JoinedLong.csv")
zip(zipfile = 'MoralMachine/Data/JoinedLong', files = "MoralMachine/Data/JoinedLong.csv")

