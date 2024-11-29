# Load functions
source("Code/RFunctions.R")


#############################################################
# Prepare US sample with demographics from Awad et al. (2018)
#############################################################

# Download data from https://osf.io/3hvt2/?view_only=4bb49492edee4a8eb1758552a362a2cf
profiles.S = get_filepath("SharedResponsesSurvey.csv.tar") %>% 
  fread() %>% 
  mutate(across(Review_age, as.numeric)) 

# Identify sessions with more than 26 rows and users who took the survey multiple times 
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
      Review_education == "underHigh" ~ "Less than\nhigh school",
      Review_education == "high" ~ "High school",
      Review_education %in% c("college","bachelor","vocational") ~ "Some college",
      Review_education == "graduate" ~ "Postgraduate") %>% 
      factor(levels=c("Less than\nhigh school","High school","Some college","Postgraduate")), 
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
    Review_political = as.integer(Review_political * 100),
    Review_religious = as.integer(Review_religious * 100),
    # Create income brackets
    IncomeBracketSmall:= case_when(
      Review_income=="under5000" ~"$0-$5,000",
      Review_income %in% c("5000","10000","15000") ~"$5,001-\n$25,000",
      Review_income %in% c("25000","35000") ~"$25,001-\n$50,000",
      Review_income %in% c("50000","80000") ~"$50,001-\n$100,000",
      Review_income=="above100000" ~"More than\n$100,000") %>% 
      factor(levels=c("$0-$5,000","5000","$5,001-\n$25,000","$25,001-\n$50,000",
                      "$50,001-\n$100,000","More than\n$100,000"))) %>% 
  data.table()


# Check that there are no missing values
summarise_all(profiles.S, ~ sum(is.na(.)))

# Number of dilemmas in thousands (we divide by 2 since there are 2 rows per observation)
nrow(profiles.S) / 10^3 / 2

# Number of respondents in thousands
length(unique(profiles.S$UserID)) / 10^3



# Download data from https://osf.io/3hvt2/?view_only=4bb49492edee4a8eb1758552a362a2cf
profiles.S.full = get_filepath("SharedResponses.csv.tar") %>% 
  fread() 

# Join columns that characterize scenario
mms = profiles.S %>% 
  distinct(ExtendedSessionID,ResponseID,UserID,Review_gender,Review_age,Review_ageBracket,
           Review_income,Review_ContinuousIncome,IncomeBracketSmall,
           Review_education,Review_educationBracket,Review_political,Review_religious) %>% 
  inner_join(profiles.S.full, by = join_by(ResponseID, ExtendedSessionID, UserID)) %>% 
  mutate(Man = as.integer(Man))

# Verify that each response ID has two rows
all(count(mms,ResponseID)$n == 2)

# Verify that there are no missing values
summarize_all(mms, ~ sum(is.na(.)))

# Save data 
# write_csv(mms,paste0(get_filepath("Data"),"/2_SurveySample.csv.gz"))


################################################################
# Prepare data from the American Community Survey for comparison 
################################################################

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
      CorrPINCP>=100000 ~"More than\n$100,000") %>% 
      factor(levels=c("$0-$5,000","$5,001-\n$25,000","$25,001-\n$50,000","$50,001-\n$100,000","More than\n$100,000")),
    
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
      SCHL<=15 ~ "Less than\nhigh school",
      SCHL %in% c(16:17) ~"High school",
      SCHL %in% c(18:21) ~"Some college",
      SCHL %in% c(22:24) ~"Postgraduate") %>% 
      factor(levels=c("Less than\nhigh school","High school","Some college","Postgraduate")))

## Calculate percentages and correct with weights
acsPerc = acs %>% 
  group_by(Gender,AgeBracket,IncomeBracketSmall,EducationBracket) %>% 
  summarise(ACSn = sum(PWGTP)) %>% 
  ungroup() %>% 
  mutate(ACSfreq = ACSn/sum(ACSn))

# Check that there are no missing values
summarize_all(acsPerc, ~ sum(is.na(.)))





################################################################################
# Compare demographic distribution of samples ----------------------------------
################################################################################

compute_dem_share = function(.acs_var,.mm_var){
  
  # American Community Survey
  acsvar = acs %>% 
    group_by({{.acs_var}}) %>% 
    summarize(n = sum(PWGTP)) %>% 
    ungroup() %>% 
    mutate(acsFreq = n / sum(n)) %>% 
    select(-n)
  
  # Moral Machine Sample
  mmsvar = mms %>% 
    group_by({{.mm_var}}) %>% 
    summarize(n = length(unique(UserID))) %>% 
    ungroup() %>% 
    mutate(mmsFreq = n / sum(n)) %>% 
    select(-n)
  
  # Join
  acsvar %>% 
    left_join(mmsvar,by=join_by({{.acs_var}} == {{.mm_var}})) %>% 
    mutate(mmsFreq = mmsFreq %>% ifelse(is.na(.),0,.), 
           Variable = colnames(select(acs,{{.acs_var}}))) %>% 
    select(Variable, everything()) %>% 
    rename(Level = {{.acs_var}})
  
}

# Calculate frequencies of demographic categories for each of the datasets
GenderFreq = compute_dem_share(Gender,Review_gender)
AgeFreq = compute_dem_share(AgeBracket,Review_ageBracket)
EducationFreq = compute_dem_share(EducationBracket,Review_educationBracket) 
IncomeFreq = compute_dem_share(IncomeBracketSmall,IncomeBracketSmall) 

# Assemble statistics and compute absolute difference in relative frequencies
FreqWide = GenderFreq %>% 
  bind_rows(AgeFreq) %>% 
  bind_rows(EducationFreq) %>% 
  bind_rows(IncomeFreq) %>% 
  mutate(absDiffmms = abs(acsFreq - mmsFreq))

# Calculate mean absolute difference in percentage points per variable
DiffPP = FreqWide %>% 
  group_by(Variable) %>% 
  summarise(AvgAbsDiffMMS = mean(absDiffmms))
DiffPP

# Mean absolute difference across variables
mean(DiffPP$AvgAbsDiffMMS)


# Create bar plot with relative frequencies of demographics
cols = tribble(
  ~var,     ~col,      ~lab, 
  "mmsFreq",  "#CC6677", "\nMoral Machine U.S. Sample\n",
  "acsFreq",  "#66aacc", "\nAmerican Community Survey 2016\n") %>% 
  mutate(lab = str_replace_all(lab," ","\n"),
         var = factor(var,ordered = T))

# Create labels for plots
labell = c(AgeBracket="Age",EducationBracket="Education",
           Gender="Gender",IncomeBracketSmall="Income")

# Convert from wide to long data format
FreqLong = FreqWide %>% 
  pivot_longer(cols = c(acsFreq,mmsFreq)) %>%
  mutate(name = factor(name,levels=cols$var))

# Create ggplot 
DemPlot = FreqLong %>% 
  ggplot(aes(Level, value, fill=name)) +
  geom_col(position = position_dodge(), width=0.5) +
  facet_wrap(~ Variable, scales = "free_x",
             labeller = labeller(Variable=labell)) +
  scale_fill_manual(breaks = cols$var, values = cols$col,labels=cols$lab) +
  labs(fill = "Dataset",x="",y="Relative frequency") +
  theme(axis.text.x = element_text(size = 9.5))

DemPlot = FreqLong %>% 
  ggplot(aes(Level, value, fill = name)) +
  geom_col(position = position_dodge(), width = 0.5) +
  facet_wrap(~ Variable, scales = "free_x",
             labeller = labeller(Variable = labell)) +
  scale_fill_manual(
    breaks = cols$var, 
    values = cols$col, 
    labels = cols$lab
  ) +
  labs(
    fill = "Dataset", 
    x = "", 
    y = "Relative frequency"
  ) 
DemPlot

# Save plot
ggsave(DemPlot,filename=paste0(get_filepath("Figures"),"/2_DemographicDistribution.pdf"),width=9,height=6)

