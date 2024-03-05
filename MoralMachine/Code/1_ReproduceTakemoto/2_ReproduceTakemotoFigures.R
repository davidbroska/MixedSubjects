library(readr)
library(dplyr)
library(data.table)
source("Code/0_Functions.R")
#source("Data/Takemoto2023/chatbot_MMFunctionsShared.R")

GPT3 = fread("Data/JoinedAwadTakemoto/Joined_GPT3.csv")
GPT4 = fread("Data/JoinedAwadTakemoto/Joined_GPT4.csv")
Llama2 = fread("Data/JoinedAwadTakemoto/Joined_Llama2.csv")
Palm2 = fread("Data/JoinedAwadTakemoto/Joined_Palm2.csv")



ComputeACME = function(.profiles, .var){
  .profiles = PreprocessProfiles(.profiles)
  # Rename variable  
  .profiles$Saved = .profiles[[which(grepl(paste0("^",.var,"$"),colnames(.profiles)))]]
  # Drop NAs 
  .profiles = .profiles[which(!is.na(.profiles$Saved)), ]
  # Calculate ACMEs
  Coeffs.main = GetMainEffectSizes(.profiles,T,9)
  plotdata.main = GetPlotData(Coeffs.main,T,9)
  
  Coeffs.util <- GetMainEffectSizes.Util(.profiles)
  plotdata.util <- GetPlotData.Util(Coeffs.util)
  
  l = list(plotdata.main, plotdata.util) 
  names(l) = c("plotdata.main","plotdata.util")
  
  return(l)
}

AMCE_GPT3 = ComputeACME(GPT3, "SavedGPT3")
AMCE_GPT4 = ComputeACME(GPT4, "SavedGPT4")
AMCE_Llama2 = ComputeACME(Llama2, "SavedLlama2")
AMCE_Palm2 = ComputeACME(Palm2, "SavedPalm2")


## plot and save
p_GPT3 = PlotAndSave(AMCE_GPT3$plotdata.main, T, "Figures/MainChangePr_GPT3", AMCE_GPT3$plotdata.util, "GPT 3.5")
p_GPT4 = PlotAndSave(AMCE_GPT4$plotdata.main, T, "Figures/MainChangePr_GPT4", AMCE_GPT4$plotdata.util, "GPT 4")
p_Llama2 = PlotAndSave(AMCE_Llama2$plotdata.main, T, "Figures/MainChangePr_Llama2", AMCE_Llama2$plotdata.util, "Llama 2")
p_Palm2 = PlotAndSave(AMCE_Palm2$plotdata.main, T, "Figures/MainChangePr_Palm2", AMCE_Palm2$plotdata.util, "Palm 2")



