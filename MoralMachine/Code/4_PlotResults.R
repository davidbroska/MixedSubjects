library(ggplot2)
library(tidyr)
library(readr)
library(dplyr)
library(stringr)
library(gridExtra)
theme_set(theme_bw(base_size = 12))


gpt4 = read_csv("MoralMachine/Data/4_PPI_GPT4.csv") %>%
  select(- ...1) %>% 
  mutate(WidthOLS = abs(upper_CI_ols - lower_CI_ols),
         WidthPPI = abs(upper_CI_ppi - lower_CI_ppi), 
         n = as.integer(n), 
         N = as.integer(N), 
         llm = "GPT4", 
         var = factor(var, levels = c("NumberOfCharacters","Girl","Woman","Boy","Man"))) %>% 
  # average over repetitions
  group_by(var,n,N) %>% 
  summarise_if(is.numeric, mean) %>% 
  mutate(ratio = WidthPPI / WidthOLS, 
         k = N/n) %>% 
  filter(k <= 20, 
         n >= 100,N >= 100,
         #var %in% c("NumberOfCharacters","Woman","Girl")
  ) 

# 1. the conditional correlation may be stronger than the unconditional one
# 2. with weights

JoinedLong = read_csv("MoralMachine/Data/JoinedLong.csv")
sum(!is.na(JoinedLong$SavedGPT4))

# correlation between LLM and human responses
JoinedLong %>% 
  select(ResponseID, Saved, SavedGPT4,SavedGPT3,SavedLlama2,SavedPalm2) %>% 
  pivot_longer(cols = c(SavedGPT4,SavedGPT3,SavedLlama2,SavedPalm2)) %>% 
  group_by(name) %>% 
  summarise(cor = cor(Saved,value, use = "complete.obs"), n = sum(!is.na(value))) %>% 
  mutate(name = str_remove(name, "Saved")) %>% 
  arrange(desc(cor))

scale2 = function(x) (x - mean(x,na.rm=T)) / (2*sd(x,na.rm=T))


# Number of characters
table(JoinedLong$Woman)
mc = lm(Saved ~ NumberOfCharacters,JoinedLong, weights = weights) 
mcllm = lm(SavedGPT4 ~ scale2(NumberOfCharacters),JoinedLong, weights = weights)
get_diff(mcllm,mc)



gpt4$pointest_ols = rowMeans(gpt4[,c("upper_CI_ols","lower_CI_ols")])

boxp_data = gpt4 %>% 
  #group_by(n,N,var) %>% 
  #summarise_if(is.numeric, mean) %>% 
  group_by(var) %>% 
  summarize(ppiy000 = min(pointest_ppi),
            ppiy0025 = quantile(pointest_ppi,0.025),
            ppiy050 = quantile(pointest_ppi,0.50),
            ppiy0975 = quantile(pointest_ppi,0.975),
            ppiy100 = max(pointest_ppi),
            olsy000 = min(pointest_ols),
            olsy0025 = quantile(pointest_ols,0.025),
            olsy050 = quantile(pointest_ols,0.50),
            olsy0975 = quantile(pointest_ols,0.975),
            olsy100 = max(pointest_ols)) %>% 
  left_join(reg, by = c("var"="term")) %>% 
  mutate(var = factor(var, levels = c("NumberOfCharacters","Girl","Woman","Boy","Man"))) 


ggplot(boxp_data, aes(x = var)) +
  geom_boxplot(aes(ymin=olsy000, lower=olsy0025, middle=olsy050, upper=olsy0975, ymax=olsy100, 
                   fill="OLS"),
               stat = "identity", width = 0.1, position= position_nudge(x=-.1)) +
  geom_boxplot(aes(ymin=ppiy000, lower=ppiy0025, middle=ppiy050, upper=ppiy0975, ymax=ppiy100, 
                   fill = "PPI"),
               stat = "identity", width = 0.1, position= position_nudge(x=+.1)) +
  geom_point(aes(var,y=estimate, fill = "Ground truth"),color = "darkred", position= position_nudge(x=0)) +
  geom_errorbar(aes(x=var,ymin=conf.low, y=estimate, ymax=conf.high), 
                  color = "darkred", width = 0.04, position= position_nudge(x=-.0)) +
  labs(title = NULL, x = NULL, y = NULL) +
  scale_fill_discrete(name = "Point estimates") +
  theme(legend.position = "bottom") 
ggsave("MoralMachine/Figures/TruePPISimulation.png", width=7, height=6)













get_diff = function(m1,m2){
  print(m2)
  d = coef(m2)[2] - coef(m1)[2]
  round(abs(d),3)
}
# Woman
table(JoinedLong$Woman)
mw = lm(Saved ~ Woman,JoinedLong, weights = weights) 
mwllm = lm(SavedGPT4 ~ Woman,JoinedLong, weights = weights) 
get_diff(mwllm,mw)

# Man
table(JoinedLong$Man)
mm = lm(Saved ~ Man,JoinedLong, weights=weights) %>% summary()
mmllm = lm(SavedGPT4 ~ Man,JoinedLong, weights=weights) %>% summary()
get_diff(mmllm,mm)

# Boy
table(JoinedLong$Boy)
mb = lm(Saved ~ Boy,JoinedLong, weights = weights) %>% summary()
mbllm = lm(SavedGPT4 ~ Boy,JoinedLong, weights = weights) %>% summary()
get_diff(mb,mbllm)

# Girl
table(JoinedLong$Girl)
mg = lm(Saved ~ Girl,JoinedLong, weights=weights) %>% summary()
mgllm = lm(SavedGPT4 ~ Girl,JoinedLong, weights=weights) %>% summary()
get_diff(mg,mgllm)

reg = lapply(list(mc,mw,mm,mb,mg), function(m) {
  broom::tidy(m,conf.int=T) %>% filter(term != "(Intercept)")}) %>% 
  do.call(bind_rows,.)



tb = tribble(~ var,              ~ true_coef,      ~ absbetadiff,
             "NumberOfCharacters", coef(mc)[2],      get_diff(mcllm,mc),
             "Woman",              coef(mw)[2],      get_diff(mwllm,mw), 
             "Man",                coef(mm)[2],      get_diff(mmllm,mm), 
             "Boy",                coef(mb)[2],      get_diff(mbllm,mb), 
             "Girl",               coef(mg)[2],      get_diff(mg,mgllm)) 

  

ggplot(gpt4, aes(k,ratio)) +
  geom_point(alpha = 0.8,size = 0.3) +
  geom_smooth(se = F, color="forestgreen")+
  facet_wrap(~ var,nrow=1) + 
  geom_text(aes(x = 10,y = 0.7, label= paste0("d=",absbetadiff)), data=tb, size = 3.5) + 
  scale_y_continuous(limits = c(0.70,NA), breaks = seq(0,2,0.1)) +
  labs(x = "Ratio of LLM simulations to human responses (N/n)", 
       y = "Ratio of PPI CI width to classical CI") 
ggsave("MoralMachine/Figures/4_ratioCIs.png")
  


gpt4 %>% 
  filter(N <= 2000) %>% 
  ggplot(aes(x=N,y=n,z=ratio)) + 
  geom_contour_filled(binwidth = .05) +
  scale_y_continuous(breaks = c(100,seq(500,1000,500))) + 
  scale_x_continuous(breaks = c(100,seq(500,2000,500))) +
  coord_fixed(ratio = 1) +
  facet_wrap(~ var,nrow = 1) +
  theme(legend.position = "bottom", 
        theme(plot.margin = unit(c(0, 0, 0, 0), "cm")),
        axis.text = element_text(size=7.5)) +
  labs(x = "LLM simulations N",
       y = "Human responses n",
       fill = "Ratio of PPI CI width to classical CI")
ggsave("MoralMachine/Figures/4_contourCIs.png")


m = lm(ratio ~ n+N+var,gpt4)
p100n = 100 * coef(m)["n"]
p100N = 100 * coef(m)["N"]
p100n / p100N

m = gpt4 %>% 
  group_by(var) %>% 
  do(tidy())
  lm(ratio ~ n + I(n^2) + I(n^3) + N + I(N^2) + I(N^3),.)
pbase = predict(m, newdata = data.frame(n=mean(gpt4$n),N=mean(gpt4$N)))
pn_plus1 = predict(m, newdata = data.frame(n=mean(gpt4$n)+1,N=mean(gpt4$N)))
pN_plot1 = predict(m, newdata = data.frame(n=mean(gpt4$n),N=mean(gpt4$N)+1))
pN_plot1-pbase

summary(m)









gpt3 = read_csv("MoralMachine/Data/2_PPI_GPT3.csv") %>% select(-...1) %>% 
  mutate(WidthOLS = abs(upper_CI_ols - lower_CI_ols),
         WidthPPI = abs(upper_CI_ppi - lower_CI_ppi), 
         WidthPPIMinOLS = WidthPPI-WidthOLS,
         n = as.integer(n), 
         N = as.integer(N),
         llm="GPT3")

df = bind_rows(gpt3,gpt4)
  

# Comparison ground truth
ns <- c(50, 100, 250, 500, 1000, 2000)
Ns <- c(50, 100, 250, 500, 1000, 2000)

dd = gpt4 %>% 
  filter(n>= 100, N>= 100) %>% 
  left_join(tb, by = "var") %>% 
  mutate(ppi_b_covers_true = ifelse(lower_CI_ppi <= true_coef & upper_CI_ppi >= true_coef, 1, 0), 
         ols_b_covers_true = ifelse(lower_CI_ols <= true_coef & upper_CI_ols >= true_coef, 1, 0)) 






summarise_if(gpt4, is.numeric, sd)
summarise(gpt4, across(c(n,N), list(min=min,max=max,mean=mean,sd=sd)))
summarise(gpt4, across(c(WidthOLS,WidthPPI), list(min=min,max=max,mean=mean,sd=sd)))

text = gpt4 %>% filter(n %in% c(250,500,750), N==7500) %>% distinct(var,n,N,WidthOLS) %>% 
  mutate(WidthOLS = round(WidthOLS,2))
maxw = max(c(max(gpt4$WidthOLS) , max(gpt4$WidthPPI)))
gpt4 %>%
  ggplot(aes(N,n, z=WidthPPI)) +
  geom_contour_filled(breaks = c(seq(0,0.2,0.025),0.5,1,maxw)) +
  facet_wrap(~ var, ncol = 7) + 
  geom_text(aes(N,n,label=WidthOLS),data = text,size =2,inherit.aes = F, color="white") + 
  labs(fill = "Width of PPI CI", y = "n of human responses",
       x = "N of GPT4 responses", title="Contour plot of width of CI across sample sizes of human responses and GPT predictions", 
       caption = "Note: The CI contour color increase in increments of 0.025 until 0.2 and then in larger increments.") 
ggsave("MoralMachine/Figures/4_ContourPlot.png",width=10,height=8)
gpt4 %>%
  filter(n <= 500, N <= 500) %>% 
  ggplot(aes(N,n, z=WidthPPI)) +
  geom_contour_filled(breaks = c(seq(0,0.2,0.025),0.5,1,maxw)) +
  facet_wrap(~ var, ncol = 7, scales = "free") + 
  geom_text(aes(N,n,label=WidthOLS),data = text,size =2,inherit.aes = F, color="white") + 
  labs(fill = "Width of PPI CI", y = "n of human responses",
       x = "N of GPT4 responses", title="Contour plot of width of CI across sample sizes of human responses and GPT predictions", 
       caption = "Note: The CI contour color increase in increments of 0.025 until 0.2 and then in larger increments.") 
ggsave("MoralMachine/Figures/4_ContourPlotSmallN.png",width=10,height=8)
# This contour shows the width of the PPI confidence interval (CI) as it varies by the number of GPT responses (N) and the number of human responses (n) with darker colors showing smaller CIs. 
# Unfortunately, the width of the PPI CI varies mostly by n and not by N. For reference, I also added the width of the OLS confidence interval for n decreases with additional n as one would expect.

nhuman = 500
gpt4 %>%
  filter(n == nhuman) %>% 
  pivot_longer(cols = c(WidthOLS,WidthPPI)) %>%
  ggplot(aes(N, value,color = name)) +
  geom_line() +
  facet_wrap(~ var, ncol=7) +
  labs(color = "Width of CI", y = "Width of CI",
       x = "N of GPT4 responses", 
       title=paste0("Width of CI across sample size of GPT predictions with n=",nhuman," human responses")) +
  theme(legend.position = "bottom")
ggsave("MoralMachine/Figures/4_CIWidthPPIandOLSacrossN.png",width=10,height=8)

m = lm(WidthPPI ~ n+N+var,gpt4)
summary(m)
10^3 * coef(m)["N"]
# assuming  $0.01 / 1K prompt tokens


breaks = c(50,250,500,1000)
m3 = lm(ratio ~ n+N+I(N^2)+I(N^3)+var,gpt3)
pred3 = broom::augment(m3) %>% 
  group_by(n,N) %>% 
  summarize(.fitted=mean(.fitted)) %>% 
  mutate(LLM = "GPT3") %>% 
  filter(n %in% breaks)


m4 = lm(ratio ~ n+N+I(N^2)+I(N^3)+var,gpt4) 
pred4 = broom::augment(m4) %>% 
  group_by(n,N) %>% 
  summarize(.fitted=mean(.fitted)) %>% 
  mutate(LLM = "GPT4") %>% 
  filter(n %in% breaks)


bind_rows(pred3,pred4)  %>% 
  ggplot(aes(N,.fitted,color=factor(n))) +
  geom_line() +
  facet_wrap(~ LLM)

gpt3$WidthPPI


library(ggeffects)
ggpredict(m3, terms = c("N[50,10000]","n [50,500,1000,2000]"), "var") %>% 
  ggplot(aes(x,predicted,color=group)) +
  geom_line() 
ggpredict(m4, terms = c("N[50,2000]","n [50,250,500,1000,2000]","var")) %>% 
  ggplot(aes(x,predicted,color=group)) +
  geom_line() +
  facet_wrap(~ facet,nrow = 7)

select(gpt3,WidthOLS,WidthPPI,WidthPPIMinOLS)

maxwd = max(df$WidthPPIMinOLS)
minwd = min(df$WidthPPIMinOLS)
gpt4 %>%
  filter(var == "Girl") %>% 
  ggplot(aes(N,n, z=WidthPPIMinOLS)) +
  geom_contour_filled() +
  facet_wrap(~ var, ncol = 7) + 
  #geom_text(aes(N,n,label=WidthPPIMinOLS),data = text,size =2,inherit.aes = F, color="white") + 
  labs(fill = "Width of PPI CI", y = "n of human responses",
       x = "N of GPT4 responses", title="Contour plot of width of CI across sample sizes of human responses and GPT predictions", 
       caption = "Note: The CI contour color increase in increments of 0.025 until 0.2 and then in larger increments.") 
ggsave("MoralMachine/Figures/4_ContourPlot.png",width=10,height=8)





