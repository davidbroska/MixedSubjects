# Load functions
source("Code/1_Functions.R")

################################################################################
# American Community Survey ----------------------------------------------------
################################################################################

# Read data with survey responses and apply exclusion criteria from Awad et al
acs = get_filepath("usa_00004.csv.gz") %>% 
  fread() %>% 
  rename_with(~ str_remove(., "US2016C_")) %>% 
  filter(
    # Filter out Puerto Rico
    ST!=72,
    AGEP >= 15,
    AGEP <  95) %>% 
  mutate(
    # Correct income for years
    CorrPINCP = as.numeric(PINCP)*as.numeric(ADJINC)/1000000,
    
    IncomeBracketSmall = case_when(
      CorrPINCP<5000 ~"$0-$5,000",
      CorrPINCP>=5000 & CorrPINCP<25000 ~"$5,001-\n$25,000",
      CorrPINCP>=25000 & CorrPINCP<50000 ~"$25,001-\n$50,000",
      CorrPINCP>=50000 & CorrPINCP<100000 ~"$50,001-\n$100,000",
      CorrPINCP>=100000 ~"More than\n$100,001") %>% 
      factor(levels=c("$0-$5,000","$5,001-\n$25,000","$25,001-\n$50,000","$50,001-\n$100,000","More than\n$100,001")),
    
    AgeBracket = case_when(AGEP>=15 & AGEP<25 ~"15-24",
                           AGEP>=25 & AGEP<35 ~"25-34",
                           AGEP>=35 & AGEP<45 ~"35-44",
                           AGEP>=45 & AGEP<55 ~"45-54",
                           AGEP>=55 & AGEP<65 ~"55-64",
                           AGEP>=65 & AGEP<75 ~"65-74",
                           AGEP>=75 & AGEP<85 ~"75-84",
                           AGEP>=85 & AGEP<95 ~"85-94") %>% 
      factor(levels=c("15-24","25-34","35-44","45-54","55-64","65-74","75-84","85-94")),
    
    Gender = case_when(SEX==1 ~"Man", SEX==2 ~"Woman") %>% 
      factor(levels=c("Man","Woman")),
    
    EducationBracket:= case_when(
      SCHL<=15 ~ "Less than high school",
      SCHL %in% c(16:17) ~"High school",
      SCHL %in% c(18:21) ~"Some college",
      SCHL %in% c(22:24) ~"Postgraduate") %>% 
      factor(levels=c("Less than high school","High school","Some college","Postgraduate")))

## Calculate percentages and correct with weights
acsPerc = acs %>% 
  group_by(Gender,AgeBracket,IncomeBracketSmall,EducationBracket) %>% 
  summarise(ACSn = sum(PWGTP)) %>% 
  ungroup() %>% 
  mutate(ACSfreq = ACSn/sum(ACSn))

# Check that there are no missings
summarize_all(acsPerc, ~ sum(is.na(.)))






################################################################################
# Awad et al. (2018) -----------------------------------------------------------
################################################################################


# Read data with survey responses
profiles.S = get_filepath("SharedResponsesSurvey.csv") %>% 
  fread() %>% 
  mutate(across(Review_age, as.numeric)) 


# A completed session is represented by at max 26 rows, and some users took the survey multiple times 
anom = profiles.S %>% 
  count(UserID,ExtendedSessionID,sort = T) %>% 
  group_by(UserID) %>% 
  mutate(nn = n()) %>% 
  ungroup() %>% 
  arrange(-n,-nn) %>% 
  filter(n > 26 | nn > 1) 

# Filter out these rows
profiles.S = anti_join(profiles.S,anom,by="UserID")

# Filter out responses with only one description per scenario
profiles.S = profiles.S %>% 
  group_by(ResponseID) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  filter(n >=2) %>% 
  select(-n) %>% 
  data.table()

 

# Preprocess variables
profiles.S = PreprocessProfiles(profiles.S)
profiles.S = AddUserColumns(profiles.S)
profiles.S = RecodeColumns(profiles.S)
profiles.S = PrepareDemColumns(profiles.S)


profiles.S = profiles.S %>% 
  # Based on post-stratification analysis script in Awad
  filter(
    !Review_education %in% c(NA,"others"),
    UserCountry3 == "USA",
    Review_age >= 15,
    Review_age <  95,
    !is.na(Review_gender),
    !is.na(Review_income), 
    !is.na(DescriptionShown),
    !is.na(LeftHand),
    # Awad only use overlapping cases for their analysis (see SI)
    ScenarioTypeStrict == ScenarioType) %>% 
  mutate(
    # Create education categories
    Review_educationBracket = case_when(
      Review_education == "underHigh" ~ "Less than high school",
      Review_education == "high" ~ "High school",
      Review_education %in% c("college","bachelor","vocational") ~ "Some college",
      Review_education == "graduate" ~ "Postgraduate") %>% 
      factor(levels=c("Less than high school","High school","Some college","Postgraduate")), 
    Review_ageBracket = case_when(
      Review_age>=15 & Review_age<25 ~"15-24",
      Review_age>=25 & Review_age<35 ~"25-34",
      Review_age>=35 & Review_age<45 ~"35-44",
      Review_age>=45 & Review_age<55 ~"45-54",
      Review_age>=55 & Review_age<65 ~"55-64",
      Review_age>=65 & Review_age<75 ~"65-74",
      Review_age>=75 & Review_age<85 ~"75-84",
      Review_age>=85 & Review_age<95 ~"75-94") %>% 
      factor(levels=c("15-24","25-34","35-44","45-54","55-64","65-74","75-84","85-94")),
    Review_gender = Review_gender %>% 
      str_replace("^male$","Man") %>% 
      str_replace("^female$","Woman") %>% 
      factor(levels=c("Man","Woman")), 
    Review_political = Review_political * 100,
    Review_religious = Review_religious * 100,
    # Create income brackets
    IncomeBracketSmall:= case_when(
      Review_income=="under5000" ~"$0-$5,000",
      Review_income %in% c("5000","10000","15000") ~"$5,001-\n$25,000",
      Review_income %in% c("25000","35000") ~"$25,001-\n$50,000",
      Review_income %in% c("50000","80000") ~"$50,001-\n$100,000",
      Review_income=="above100000" ~"More than\n$100,001") %>% 
      factor(levels=c("$0-$5,000","5000","$5,001-\n$25,000","$25,001-\n$50,000",
                      "$50,001-\n$100,000","More than\n$100,001"))) %>% 
  data.table()


# Check that there are no missings
summarise_all(profiles.S, ~ sum(is.na(.)))

# Number of rows in millions
nrow(profiles.S) / 10^6



################################################################################
# Stratified sample ------------------------------------------------------------
################################################################################



# We expect 320=2*8*5*4 unique combinations, and these are available in ACS
acsPerc %>% 
  distinct(Gender,AgeBracket,IncomeBracketSmall,EducationBracket) %>% 
  nrow()

# Set sample size higher than the target of 2,000 because not all quotas will be matched exactly
nobs0 = 2500

# Get distinct combinations of categories and calculate frequencies
mmPerc = profiles.S %>% 
  # Collect data on users that match a stratum
  distinct(UserID,
           Review_gender,IncomeBracketSmall,Review_educationBracket,Review_ageBracket) %>% 
  group_by(Review_gender,IncomeBracketSmall,Review_educationBracket,Review_ageBracket) %>% 
  summarize(UserIDs = paste0(UserID,collapse = ","), MMn=n()) %>% 
  ungroup() %>% 
  mutate(MMfreq = MMn / sum(MMn))

# Not all combinations of categories from ACS are in moral machine data
nrow(acsPerc)
nrow(mmPerc)


# Keep only categories that are in both datasets and re-calculate the relative frequency in ACS 
FreqMerged = mmPerc %>% 
  left_join(acsPerc,by=c("Review_gender"="Gender","Review_ageBracket"="AgeBracket",
                         "IncomeBracketSmall","Review_educationBracket"="EducationBracket")) %>% 
  mutate(
    # Rescale relative frequencies because some combinations were dropped in join
    ACSfreqResc = ACSfreq / sum(ACSfreq),
    # Calculate expected number of people to sample from each stratum from ACS
    ExpCount = round(ACSfreqResc * nobs0), 
    # Calculate sample size within each stratum
    SampleSize = ExpCount %>% 
      ifelse(.==0,1,.) %>%     # Include at least one person from each stratum
      ifelse(. > MMn, MMn, .)  # Match expected counts as close as possible
  )

# Frequencies sum up to 1
sum(FreqMerged$ACSfreqResc)

# Get sampled user IDs
set.seed(24042024)
UserIDs = FreqMerged %>% 
  mutate(
    UserIDs = UserIDs %>% str_split(","),
    samp = purrr::map2(.x = UserIDs, .y= SampleSize, 
                       .f = function(.x, .y) sample(.x, size=.y,replace=F))) %>% 
  unnest(cols=samp) %>% 
  pull(samp)

# Frequencies sum up to 1
sum(FreqMerged$ACSfreqResc)

# Effective sample size 
length(UserIDs)
length(unique(UserIDs))

# Create moral machine sample  
mms_sample = filter(profiles.S, UserID %in% UserIDs)
  
# Check that each response ID has two rows
all(count(mms_sample,ResponseID)$n == 2)



################################################################################
# Join columns that characterize scenario --------------------------------------
################################################################################

profiles.S.full = get_filepath("SharedResponses.csv") %>% 
  fread() 

mms = mms_sample %>% 
  distinct(ExtendedSessionID,ResponseID,UserID,Review_gender,Review_age,Review_ageBracket,
           Review_income,Review_ContinuousIncome,IncomeBracketSmall,
           Review_education,Review_educationBracket,Review_political,Review_religious) %>% 
  inner_join(profiles.S.full, by = join_by(ResponseID, ExtendedSessionID, UserID)) %>% 
  mutate(Man = as.integer(Man))

# Check that each response ID has two rows
all(count(mms,ResponseID)$n == 2)

# Check that there are no missings
summarize_all(mms, ~ sum(is.na(.)))

write_csv(mms,paste0(get_filepath("Data"),"/3_SurveySample.csv"))


################################################################################
# Benchmark results ------------------------------------------------------------
################################################################################

compute_dem_share = function(.acs_var,.mm_var){
  
  acsvar = acs %>% 
    group_by({{.acs_var}}) %>% 
    summarize(n = sum(PWGTP)) %>% 
    ungroup() %>% 
    mutate(acsFreq = n / sum(n)) %>% 
    select(-n)
  
  mmsvar = mms %>% 
    group_by({{.mm_var}}) %>% 
    summarize(n = length(unique(UserID))) %>% 
    ungroup() %>% 
    mutate(mmsFreq = n / sum(n)) %>% 
    select(-n)
  
  mmvar = profiles.S %>% 
    group_by({{.mm_var}}) %>% 
    summarize(n = length(unique(UserID))) %>% 
    ungroup() %>% 
    mutate(mmFreq = n / sum(n)) %>% 
    select(-n)
  
  acsvar %>% 
    left_join(mmvar,by=join_by({{.acs_var}}  == {{.mm_var}})) %>% 
    left_join(mmsvar,by=join_by({{.acs_var}} == {{.mm_var}})) %>% 
    mutate(mmFreq = mmFreq %>% ifelse(is.na(.),0,.),
           mmsFreq = mmsFreq %>% ifelse(is.na(.),0,.), 
           Variable = colnames(select(acs,{{.acs_var}}))) %>% 
    select(Variable, everything()) %>% 
    rename(Level = {{.acs_var}})
  
}

# Calculate frequencies of demographic categories for each of the three datasets
GenderFreq = compute_dem_share(Gender,Review_gender)
AgeFreq = compute_dem_share(AgeBracket,Review_ageBracket)
EducationFreq = compute_dem_share(EducationBracket,Review_educationBracket) 
IncomeFreq = compute_dem_share(IncomeBracketSmall,IncomeBracketSmall) 


cols = tribble(
  ~var,     ~col,      ~lab, 
  "mmsFreq",  "#DDCC77", " Stratified Moral Machine Sample ",
  "acsFreq",  "#CC6677", " American Community Survey ",
  "mmFreq",   "#4477AA", " Raw Moral Machine Sample ") %>% 
  mutate(lab = str_replace_all(lab," ","\n"),
         var = factor(var,ordered = T))
labell = c(AgeBracket="Age",EducationBracket="Education",
           Gender="Gender",IncomeBracketSmall="Income")

FreqWide = GenderFreq %>% 
  bind_rows(AgeFreq) %>% 
  bind_rows(EducationFreq) %>% 
  bind_rows(IncomeFreq) %>% 
  mutate(absDiffmm = abs(acsFreq - mmFreq),
         absDiffmms = abs(acsFreq - mmsFreq))

FreqLong = FreqWide %>% 
  pivot_longer(cols = c(acsFreq,mmFreq,mmsFreq)) %>%
  mutate(name = factor(name,levels=cols$var))

FreqLong %>% 
  ggplot(aes(Level, value, fill=name)) +
  geom_col(position = position_dodge(),width=0.5) +
  facet_wrap(~ Variable, scales = "free_x",
             labeller = labeller(Variable=labell)) +
  scale_fill_manual(breaks = cols$var, values = cols$col,labels=cols$lab) +
  labs(fill = "Dataset",x="Level of variable",y="Relative frequency") +
  theme(axis.text.x = element_text(size = 8.7))
ggsave(filename=paste0(get_filepath("Figures"),"/3_DemographicDistribution.png"),width=9,height=6)

# Calculate mean absolute difference in percentage points
DiffPP = FreqWide %>% 
  group_by(Variable) %>% 
  summarise(AvgAbsDiffMMS = mean(absDiffmms),
            AvgAbsDiffMM = mean(absDiffmm), 
            Improvement = mean(absDiffmm - absDiffmms))
DiffPP

# Mean improvement in matching the census quotas relative to the MM data: 0.084pp
mean(DiffPP$Improvement)


# Joined -----------------------------------------------------------------------  

# Set up
library(tidyverse)
library(reshape2)
library(AER)
library(sandwich)
library(multiwayvcov)
library(data.table)
library(hrbrthemes)
library(extrafont)
# font_import()
# y
fonts()
#loadfonts()
library(ggthemes)

PreprocessProfiles = function(profiles){
  profiles[,Saved := as.numeric(Saved)]
  profiles[,ScenarioType := as.factor(ScenarioType)]
  profiles[,AttributeLevel := factor(AttributeLevel, 
                                     levels=c("Rand",
                                              "Male","Female",
                                              "Fat","Fit",
                                              "Low","High",
                                              "Old","Young",
                                              "Less","More",
                                              "Pets","Hoomans"))]
  profiles[,Barrier := factor(Barrier, levels=c(1,0))]
  profiles[,CrossingSignal := factor(CrossingSignal, levels=c(0,2,1))]
  profiles[,ScenarioType := as.factor(ScenarioType)]
  profiles[,ScenarioTypeStrict := as.factor(ScenarioTypeStrict)]
  return(profiles)
}



calcWeightsActual = function(Tr, X){
  T10 = ifelse(Tr==levels(factor(Tr))[2],1,0)
  d = as.numeric(ave(X, X, T10, FUN = length))
  w = max(d)/d
  return(w)
}

calcWeightsTheoretical = function(profiles){
  p = apply(profiles,1,CalcTheoreticalInt)
  return(1/p)
}

CalcTheoreticalInt = function(X){
  if (X["Intervention"]==0){
    if (X["Barrier"]==0){
      if (X["PedPed"] == 1) p = 0.48
      else p = 0.32
      
      if (X["CrossingSignal"]==0) p = p*0.48
      else if (X["CrossingSignal"]==1) p = p*0.2
      else p = p * 0.32
    }
    else p = 0.2
  }
  else {
    if (X["Barrier"]==0){
      if (X["PedPed"] == 1) {
        p = 0.48
        if (X["CrossingSignal"]==0) p = p*0.48
        else if (X["CrossingSignal"]==1) p = p*0.32
        else p = p * 0.2
      }
      else {
        p = 0.2
        if (X["CrossingSignal"]==0) p = p*0.48
        else if (X["CrossingSignal"]==1) p = p*0.2
        else p = p * 0.32
      }
    }
    else p = 0.32
  }
  return(p)
}


########################################
############ Main Effects ##############
########################################
# Main effect sizes
GetMainEffectSizes = function(profiles,savedata,r,depvar="Saved"){
  profiles$dv = profiles[[depvar]]
  Coeffs = matrix(NA,r,2)
  AttLevels = levels(profiles$AttributeLevel)
  lev = levels(profiles$ScenarioType)
  if (levels(profiles$ScenarioType)[1]=="") lev = levels(profiles$ScenarioType)[2:8]
  #lev = lev[c(3,2,4,1,6,5)]
  lev = c("Gender", "Fitness", "Social Status", "Age", "Utilitarian", "Species")
  #gender fitness social age uti spec
  # Six factors (gender, fitness, Social Status, age, utilitarianism, age, and species)
  
  # For intervention
  profiles$BC.weights = calcWeightsTheoretical(profiles)
  lm.Int = lm(dv ~as.factor(Intervention), data=profiles, weights = BC.weights)
  summary.lm.Int = summary(lm.Int)
  Coeffs[1,1] = lm.Int$coefficients[[2]]
  Coeffs[1,2] = summary.lm.Int$coefficients[2, "Std. Error"]
  
  # For relationship to vehicle 
  ## Consider only 'no legality' (CrossingSignal==0) and 'passengers vs. pedestrians' (PedPed==0)
  profile.Relation = profiles[which(profiles$CrossingSignal==0 & profiles$PedPed==0),]
  profile.Relation$BC.weights = calcWeightsTheoretical(profile.Relation)
  lm.Rel = lm(dv ~as.factor(Barrier), data=profile.Relation, weights = BC.weights)
  summary.lm.Rel = summary(lm.Rel)
  Coeffs[2,1] = lm.Rel$coefficients[[2]]
  Coeffs[2,2] = summary.lm.Rel$coefficients[2, "Std. Error"]
  
  # Legality 
  ## Exclude 'no legality' (CrossingSignal!=0) and consider only 'pedestrians vs. pedestrians' (PedPed==1)
  profile.Legality = profiles[which(profiles$CrossingSignal!=0 & profiles$PedPed==1),]
  profile.Legality$CrossingSignal = factor(profile.Legality$CrossingSignal,
                                            levels=levels(profiles$CrossingSignal)[2:3])
  profile.Legality$BC.weights = calcWeightsTheoretical(profile.Legality)
  lm.Leg = lm(dv ~as.factor(CrossingSignal), data=profile.Legality, weights = BC.weights)
  summary.lm.Leg = summary(lm.Leg)
  Coeffs[3,1] = lm.Leg$coefficients[[2]]
  Coeffs[3,2] = summary.lm.Leg$coefficients[2, "Std. Error"]
  
  # Six factors (gender, fitness, Social Status, age, utilitarianism, age, and species)
  ## Extract data subsets and run regression for each
  for(i in 1:6){
    Temp = profiles[which(profiles$ScenarioType==lev[i] & profiles$ScenarioTypeStrict==lev[i]),]
    Temp$AttributeLevel = factor(Temp$AttributeLevel, levels=AttLevels[(i*2):(i*2+1)])
    Temp$BC.weights = calcWeightsTheoretical(Temp)    
    lm.Temp = lm(dv ~ as.factor(AttributeLevel), data=Temp, weights = BC.weights)
    summary.lm.Temp = summary(lm.Temp)
    #print(summary.lm.Temp)
    Coeffs[i+3,1] = lm.Temp$coefficients[[2]]
    Coeffs[i+3,2] = summary.lm.Temp$coefficients[2, "Std. Error"]
    
    # Save to a data frame
    if(savedata){
      var.name = paste("profile",gsub(" ","",lev[i]),sep=".")
      assign(var.name,Temp)
    }
  }
  return(Coeffs)
}

# Prepare plotted dataset
GetPlotData = function(Coeffs,isMainFig,r){
  # Convert to dataframe and add labels
  plotdata = as.data.frame(Coeffs)
  colnames(plotdata)=c("Estimates","se")
  plotdata$Label = c("Preference for action -> \n Preference for inaction",
                      "Sparing Passengers -> \n Sparing Pedestrians",
                      "Sparing the Unlawful -> \n Sparing the Lawful",
                      "Sparing Males -> \n Sparing Females",
                      "Sparing the Large -> \n Sparing the Fit",
                      "Sparing Lower Status -> \n Sparing Higher Status",
                      "Sparing the Elderly -> \n Sparing the Young",
                      "Sparing Fewer Characters -> \n Sparing More Characters",
                      "Sparing Pets -> \n Sparing Humans") 
  if(isMainFig)
    plotdata$Label = c("Intervention",
                        "Relation to AV",
                        "Law",
                        "Gender",
                        "Fitness",
                        "Social Status",
                        "Age",
                        "No. Characters",
                        "Species") 
  
  
  plotdata$Label = factor(plotdata$Label,as.ordered(plotdata$Label[match(sort(plotdata$Estimates[1:r]),
                                                                          plotdata$Estimates[1:r])]))
  plotdata$Label = factor(plotdata$Label,levels = rev(levels(plotdata$Label)))
  
  plotdata$Estimates = as.numeric(as.character(plotdata$Estimates))
  plotdata$se = as.numeric(as.character(plotdata$se))
  
  return(plotdata)
}

# Effect of difference in number of characters within Utilitarian dimension
## Subclass by diff in number of characters
GetMainEffectSizes.Util = function(profiles,depvar="Saved"){
  profiles$dv = profiles[[depvar]]
  Coeffs = matrix(NA,4,2)
  AttLevels = levels(profiles$AttributeLevel)
  for(i in 1:4){
    Temp = profiles[which(profiles$ScenarioType== "Utilitarian" & 
                           profiles$ScenarioTypeStrict== "Utilitarian" & 
                           profiles$DiffNumberOFCharacters==i),]
    Temp$AttributeLevel = factor(Temp$AttributeLevel, levels=AttLevels[10:11])
    Temp$BC.weights = calcWeightsTheoretical(Temp)
    lm.Signed.NoC.Util = lm(dv ~as.factor(AttributeLevel), data=Temp, weights = BC.weights)
    summary.lm.Signed.NoC.Util = summary(lm.Signed.NoC.Util)
    Coeffs[i,1] = lm.Signed.NoC.Util$coefficients[[2]]
    Coeffs[i,2] = summary.lm.Signed.NoC.Util$coefficients[2, "Std. Error"]
  }
  return(Coeffs)
}

GetPlotData.Util = function(Coeffs){
  plotdata = as.data.frame(Coeffs)
  colnames(plotdata)=c("Estimates","se")
  plotdata$Variant = c(1:4)
  plotdata$Variant = factor(plotdata$Variant,levels=rev(plotdata$Variant))
  plotdata$Label = as.factor(rep("No. Characters",4))
  return(plotdata)
}

PlotAndSave = function(plotdata.main,isMainFig,filename,plotdata.util, .title = ""){
  #label_order = c("Intervention", "Relation to AV", "Gender", "Fitness", 
  #"Social Status", "Law", "Age", "No. Character", "Species")
  #plotdata.bars$Label = factor(plotdata.bars$Label, levels = label_order)
  #plotdata.points$Label = factor(plotdata.points$Label, levels = label_order)
  #plotdata.util$Label = factor(plotdata.util$Label, levels = label_order)
  
  plotdata.main.human = data.frame(
    Estimates = c(0.061, 0.097, 0.353, 0.119, 0.160, 0.345, 0.497, 0.651, 0.585),
    Label = c("Intervention", "Relation to AV", "Law", "Gender", "Fitness", 
              "Social Status", "Age", "No. Characters", "Species")
  )
  
  plotdata.bars = plotdata.main[plotdata.main$Label != "No. Characters", ]
  plotdata.points = plotdata.main[plotdata.main$Label == "No. Characters", ]
  
  gg = ggplot() +
  geom_col(data = plotdata.bars, aes(x=Label, y=Estimates), width=0.5, fill = "gray", color = "black") +
  geom_errorbar(data = plotdata.bars, aes(x=Label, ymin=Estimates-se, ymax=Estimates+se), width = 0.2) +
  geom_col(data = plotdata.util[abs(plotdata.util$Estimates) == max(abs(plotdata.util$Estimates)),], 
           aes(x = Label, y = Estimates), fill = "gray", color = "black", width = 0.5) +
  geom_errorbar(data = plotdata.points, aes(x=Label, ymin=Estimates-se, ymax=Estimates+se), width= 0.2) +
  geom_point(data = plotdata.points, aes(x=Label, y=Estimates), color = "black", size = 3) +
  geom_errorbar(data = plotdata.util, aes(x = Label, ymin = Estimates - se, ymax = Estimates + se), width = 0.2) +
  geom_point(data = plotdata.util, aes(x = Label, y = Estimates), size = 3, color = "black", fill = "white", shape=21) +
  geom_text(data = plotdata.util, aes(x = Label, y = Estimates, label = Variant), hjust = 0.5, vjust = 0.5, size = 3, color = "black") +
  geom_hline(yintercept = 0, linetype="solid", color = "black", linewidth=0.4) +
  geom_point(data = plotdata.main.human, aes(x = Label, y = Estimates), color = "red", size = 3, shape = '|') +
  scale_y_continuous(limits = c(-0.5, 1.2),breaks = seq(0, 1, .2)) +
  xlab("") +
  ylab(expression(paste("\n",Delta,"P"))) +
  coord_flip() +
  annotate("text", x = "Intervention", y = -0.5, label = "Action", hjust = "left", vjust = "center", color = "black", size = 3) +
  annotate("text", x = "Intervention", y = 1.2, label = "Inaction", hjust = "right", vjust = "center", color = "black", size = 3) +
  annotate("text", x = "Relation to AV", y = -0.5, label = "Passengers", hjust = "left", vjust = "center", color = "black", size = 3) +
  annotate("text", x = "Relation to AV", y = 1.2, label = "Pedestrians", hjust = "right", vjust = "center", color = "black", size = 3) +
  annotate("text", x = "Law", y = -0.5, label = "Unlawful", hjust = "left", vjust = "center", color = "black", size = 3) +
  annotate("text", x = "Law", y = 1.2, label = "Lawful", hjust = "right", vjust = "center", color = "black", size = 3) +
  annotate("text", x = "Gender", y = -0.5, label = "Males", hjust = "left", vjust = "center", color = "black", size = 3) +
  annotate("text", x = "Gender", y = 1.2, label = "Females", hjust = "right", vjust = "center", color = "black", size = 3) +
  annotate("text", x = "Fitness", y = -0.5, label = "Large", hjust = "left", vjust = "center", color = "black", size = 3) +
  annotate("text", x = "Fitness", y = 1.2, label = "Fit", hjust = "right", vjust = "center", color = "black", size = 3) +
  annotate("text", x = "Social Status", y = -0.5, label = "Low status", hjust = "left", vjust = "center", color = "black", size = 3) +
  annotate("text", x = "Social Status", y = 1.2, label = "High status", hjust = "right", vjust = "center", color = "black", size = 3) +
  annotate("text", x = "Age", y = -0.5, label = "Old", hjust = "left", vjust = "center", color = "black", size = 3) +
  annotate("text", x = "Age", y = 1.2, label = "Young", hjust = "right", vjust = "center", color = "black", size = 3) +
  annotate("text", x = "No. Characters", y = -0.5, label = "Few", hjust = "left", vjust = "center", color = "black", size = 3) +
  annotate("text", x = "No. Characters", y = 1.2, label = "More", hjust = "right", vjust = "center", color = "black", size = 3) +
  annotate("text", x = "Species", y = -0.5, label = "Pets", hjust = "left", vjust = "center", color = "black", size = 3) +
  annotate("text", x = "Species", y = 1.2, label = "Humans", hjust = "right", vjust = "center", color = "black", size = 3) +
  theme(
    axis.text.y = element_text(angle = 45, hjust = 1),
    aspect.ratio = 0.5,
    axis.title = element_text(size = 10, color="black"),
    axis.text = element_text(size = 10, color="black")) +
    labs(subtitle = .title) +
    theme_bw(base_line_size = 0.2)
  
  ggsave(paste0(filename, ".png"), plot = gg, width = 9, height = 6)
  gg
}






computeACME = function(.profiles,.depvar){
  
  profiles = PreprocessProfiles(.profiles)
  
  profiles = profiles[ which(!is.na(profiles[[.depvar]])) ]
  print(paste0("Number of observations: ",nrow(profiles)))
  
  # Compute ACME values for joined data
  Coeffs.main = GetMainEffectSizes(profiles,T,9, depvar=.depvar)
  plotdata.main = GetPlotData(Coeffs.main,T,9)
  
  # Compute additional ACME values
  Coeffs.util = GetMainEffectSizes.Util(profiles,depvar=.depvar)
  plotdata.util = GetPlotData.Util(Coeffs.util)
  
  l = list(plotdata.main, plotdata.util)
  names(l) = c("main","util")
  return(l)
}

Saved = computeACME(data.table(mms),"Saved")
Saved
ptitle = paste0("Human respondents (",length(unique(mms$UserID))," US respondents, ", length(unique(mms$ResponseID))," Decisions)")
pSaved = PlotAndSave(Saved$main, T, paste0(get_filepath("Figures"),"/3_AMCE_StratifiedSample"), Saved$util,ptitle)
pSaved




