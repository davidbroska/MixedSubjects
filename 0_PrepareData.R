library(dplyr)
library(data.table)
rm(list = ls())
Cols <- c("AttributeLevel", "ScenarioTypeStrict", "ScenarioType", 
          "Intervention", "PedPed", "Barrier", "CrossingSignal",
          "NumberOfCharacters", "DiffNumberOFCharacters", "LeftHand",
          "Man", "Woman", "Pregnant", "Stroller", "OldMan", "OldWoman", 
          "Boy", "Girl", "Homeless", "LargeWoman", "LargeMan", "Criminal", 
          "MaleExecutive", "FemaleExecutive", "FemaleAthlete", "MaleAthlete", 
          "FemaleDoctor", "MaleDoctor", "Dog", "Cat","Saved")

ColTypes = rep("character",length(Cols))
names(ColTypes) = Cols

# GPT4 predicted responses
GPT4 = fread("Data/Takemoto2023/data/shared_responses_gpt-4-0613.csv", select = c(ColTypes,ResponseID="character"))
setnames(GPT4, "Saved", "SavedGPT4")

# GPT3 predicted responses
GPT3 = fread("Data/Takemoto2023/data/shared_responses_gpt-3.5-turbo-0613.csv", select = ColTypes)
setnames(GPT3, "Saved", "SavedGPT3")

# Llama 2 predicted responses
Llama2 = fread("Data/Takemoto2023/data/shared_responses_llama-2-7b-chat.csv", select = ColTypes)
setnames(Llama2, "Saved", "SavedLlama2")

# Llama 2 predicted responses
Palm2 = fread("Data/Takemoto2023/data/shared_responses_palm2.csv", select = ColTypes)
setnames(Palm2, "Saved", "SavedPalm2")

# Read moral machine dataset
SharedResponses = fread("Data/Awad2018/Data/Moral Machine Data/SharedResponses.csv",
                        select = c(ColTypes,UserCountry3="character"))
# Keep US respondents
SharedResponses = SharedResponses[UserCountry3 == "USA"]

# Drop country variable
SharedResponses = SharedResponses[,-"UserCountry3"]

# Keep unique rows
SharedResponses = unique(SharedResponses, by = Cols[-which(Cols=="Saved")])

# Join on Xs 
JoinVars = colnames(GPT4)[-which(colnames(GPT4) %in% c("SavedGPT4","ResponseID"))]

Joined_GPT4 = merge(GPT4, SharedResponses, by = JoinVars, all.x = T)
Joined_GPT3 = merge(GPT3, SharedResponses, by = JoinVars, all.x = T)
Joined_Llama2 = merge(Llama2, SharedResponses, by = JoinVars, all.x = T)
Joined_Palm2 = merge(Palm2, SharedResponses, by = JoinVars, all.x = T)

# count number of ground truth data points
sum(!is.na(Joined_GPT4$Saved))
sum(!is.na(Joined_GPT3$Saved))
sum(!is.na(Joined_Llama2$Saved))
sum(!is.na(Joined_Palm2$Saved))

# save to file
fwrite(Joined_GPT4, "Data/JoinedAwadTakemoto/Joined_GPT4.csv")
fwrite(Joined_GPT3, "Data/JoinedAwadTakemoto/Joined_GPT3.csv")
fwrite(Joined_Llama2, "Data/JoinedAwadTakemoto/Joined_Llama2.csv")
fwrite(Joined_Palm2, "Data/JoinedAwadTakemoto/Joined_Palm2.csv")

