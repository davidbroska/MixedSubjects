# Load packages and custom functions
source("Code/1_Functions.R")


# Load survey sample data
#ds = read_csv("Data/5_SurveySampleLLM.csv.gz")
ds = read_csv("https://raw.githubusercontent.com/davidbroska/IntegrativeExperimentsGAI/main/Data/5_SurveySampleLLM.csv.gz")

# Load results from PPI analysis
dr = read_csv("https://raw.githubusercontent.com/davidbroska/IntegrativeExperimentsGAI/main/Data/6_ResultsPPI.csv.gz")

Xs = c("Woman","Man","Boy","Girl","NumberOfCharacters")
Ys = unique(dr$y)

###################################
# Estimate ACMEs for human subjects
###################################


# Convert data to long format and estimate AMCE
amce_human = ds %>% 
  select(all_of(c("Saved",Xs,"weights"))) %>% 
  pivot_longer(cols = all_of(Xs), names_to="Predictor", values_to = "PredictorValue") %>% 
  group_by(Predictor) %>% 
  do(broom::tidy(lm(Saved ~ PredictorValue, weights=weights, data=.),conf.int=T)) %>% 
  filter(term=="PredictorValue") %>% 
  mutate(SavedType = "human_subjects")


# Same result can obtained step by step, e.g. Woman
mw = lm(Saved ~ Woman, data=ds, weights=weights) 
summary(mw)

filter(amce_human, Predictor=="Woman")

  
#####################################
# Estimate AMCEs for silicon subjects
#####################################

# Convert data to long format and estimate AMCE
amce_silicon = ds %>% 
  select(all_of(c(Ys,Xs,"weights"))) %>% 
  pivot_longer(cols = all_of(Ys), names_to="SavedType",values_to="SavedValue") %>% 
  pivot_longer(cols = all_of(Xs), names_to="Predictor", values_to = "PredictorValue") %>% 
  group_by(SavedType,Predictor) %>% 
  do(broom::tidy(lm(SavedValue ~ PredictorValue, weights=weights, data=.),conf.int=T)) %>% 
  filter(term=="PredictorValue")

amce = bind_rows(amce_silicon,amce_human)





###############################
# Comparison of point estimates
###############################

dr_avg = dr %>% 
  mutate(WidthOLS = abs(upper_CI_ols - lower_CI_ols),
         WidthPPI = abs(upper_CI_ppi - lower_CI_ppi), 
         n = as.integer(n), 
         N = as.integer(N)) %>% 
  # average over repetitions
  group_by(x,y,n,N) %>% 
  summarise_if(is.numeric, mean) %>% 
  mutate(ratio = WidthPPI / WidthOLS, k = N/n) %>% 
  # figure out why there are NAs
  filter(!is.na(pointest_ppi))

# Create
boxp_data = dr_avg %>% 
  group_by(x,y) %>% 
  summarize(ppiy000 = min(pointest_ppi),
            ppiy0025 = quantile(pointest_ppi,0.025),
            ppiy050 = quantile(pointest_ppi,0.50),
            ppiy0975 = quantile(pointest_ppi,0.975),
            ppiy100 = max(pointest_ppi))  


ggplot(boxp_data, aes(x=x)) +
  # PPI point estimates in box plot
  geom_boxplot(aes(ymin=ppiy000, lower=ppiy0025, middle=ppiy050, upper=ppiy0975, ymax=ppiy100, fill = "PPI"),
               stat = "identity", width = 0.1, position= position_nudge(x=+.2)) +
  # AMCE human subjects
  geom_pointrange(aes(Predictor,y=estimate,ymin=conf.low,ymax=conf.high,fill="Human subjects"),
                  color="darkred", position=position_nudge(x=-0.2),size=.1,data=amce_human) +
  # AMCE silicon subjects
  geom_pointrange(aes(Predictor,y=estimate,ymin=conf.low,ymax=conf.high,shape=SavedType),
                  position=position_dodge(width = .2),size=.1,data=amce_silicon) +
  labs(title = NULL, x = NULL, y = NULL,shape=NULL) +
  scale_fill_discrete(name = "Point estimates") +
  theme(legend.position = "bottom") +
  facet_wrap(~ y)
ggsave("Figures/8_TruePPISimulation.png", width=7, height=6)





