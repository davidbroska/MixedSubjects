

library(data.table)
library(dplyr)
# We are going to join on all of the variables below except for Intervention and Saved
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

Rows2 = fread("MoralMachine/Data/Temp/SharedResponses2Rows.csv", select="ResponseID",key="ResponseID")

SharedResponses = fread("MoralMachine/Data/Awad2018/Data/Moral Machine Data/SharedResponses.csv",
                        select=c(Covs,"Saved","ResponseID"), key = "ResponseID")
SharedResponses = na.omit(SharedResponses)

SharedResponsesClean = semi_join(SharedResponses, Rows2, by = "ResponseID")

lapply(SharedResponsesClean, function(x) sum(is.na(x)))
which(SharedResponsesClean$Man == "(nan, nan, nan, nan)")
SharedResponsesClean$Man = as.integer(SharedResponsesClean$Man)

fwrite(SharedResponsesClean, "MoralMachine/Data/SharedResponsesClean.csv")

zip(zipfile = 'MoralMachine/Data/SharedResponsesClean', files = "MoralMachine/Data/SharedResponsesClean.csv")
