library(ggplot2)
library(rstanarm)
library(rstan)
library(tidyverse)
library(ggpubr)
#library(sqldf)
library(gridExtra)

# Functions from MM Repository (not sure if the weights generalize to LLM data)

calcWeightsTheoretical <- function(profiles){
  p <- apply(profiles,1,CalcTheoreticalInt)
  return(1/p)
}

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

# Set the working directory (you will need to change)
#setwd("C:/Users/Austin/OneDrive - Stanford/Of Mice and Machine")

# I'm loading a random smaple of the MM data (ideally we could do the whole file, but it takes forever)
d_human = list.dirs() %>% 
  lapply(function(d) list.files(d, recursive = T)) %>%
  unlist() %>% 
  str_subset("small_humans.csv") %>% 
  {{.[1]}} %>% 
  read_csv() %>% 
  filter(!is.na(Intervention))


d_human$llm <- 0
d_human$weights <- calcWeightsTheoretical(d_human) # Calculate weights

# Load the Takemoto data
gpt4_path = list.dirs() %>% 
  lapply(function(d) list.files(d, recursive = T)) %>%
  unlist() %>% 
  str_subset("shared_responses_gpt-4-0613.csv") 

d_llm <- read_csv(gpt4_path[1])
d_llm$llm <- 1
d_llm$weights <- calcWeightsTheoretical(d_llm) # Calculate weights

options(mc.cores = parallel::detectCores()) # Faster Stan

brm_mm_test <- function(n, N, delta, x) {
  
  # Given a number of human participants (n), a number of LLm participants (N),
  # a hyperparameter value (delta) that dictates how much to "trust" LLM responses,
  # an an independent variable (x), calculates the various statistics we want
  # to test.
  
  # Create a random sample from our larger data with n human responses and
  # N LLM responses.
  
  d.h.test <- sample_n(d_human, n)
  
  d.test <- rbind(d.h.test,
                  sample_n(d_llm, N))
  
  w <<- d.test$weights # For some reason I have to do this here
  
  # Fit a Bayesian linear regression in which Saved is regressed on x.
  # Priors are set on each, with all means of priors set at 0. Scales on the
  # prior distributions are hard-coded for now, besides for the interaction
  # term. This is a hyperparameter value set by the function.
  f <- paste0('Saved ~ ', x, ' + llm + Intervention:llm')
  br <- stan_glm(f, data=d.test, family=gaussian, weights=as.numeric(w),
                 prior = normal(c(0, 0, 0), 
                                c(0.25, 0.25, delta), 
                                autoscale = TRUE))

  # Sample from the posterior of the treatment effect of X on Saved
  post.samps <- as.data.frame(br, pars=c(x))[,x]
  est <- mean(post.samps) # MAP
  ci <- quantile(post.samps, probs=c(0.025, 0.975)) # 95% credible interval
  ci.width <- as.numeric(abs(ci[2] - ci[1])) # Width of 95% credible interval
  
  # Get human-only data
  
  w.h <<- d.h.test$weights
  
  f.h <- paste0('Saved ~ ', x)
  br.h <- stan_glm(f.h, data=d.h.test, family=gaussian, weights=as.numeric(w.h),
                   prior = normal(0, 0.25))
  post.samps.h <- as.data.frame(br.h, pars=c(x))[,x]
  est.h <- mean(post.samps.h) # posterior mean 
  ci.h <- quantile(post.samps.h, probs=c(0.025, 0.975)) # posterior 95% interval 
  ci.h.width <- as.numeric(abs(ci.h[2] - ci.h[1]))
  
  return(data.frame(diff.width.ci = as.numeric(ci.h.width - ci.width),
           est = as.numeric(est),
           est.h = as.numeric(est.h),
           n = as.numeric(n),
           N = as.numeric(N),
           delta = as.numeric(delta),
           x = as.character(x)))
}


df.intervention <- data.frame(diff.width=as.numeric(),
                              est = as.numeric(), 
                              est.h = as.numeric(),
                              n = as.numeric(),
                              N = as.numeric(),
                              delta = as.numeric())

ns <- c(50, 100, 250, 500, 1000, 2000)
Ns <- c(50, 100, 250, 500, 1000, 2000)
deltas <- c(0.01, 0.1, 0.2, 0.3)
repeats <- 75

total <- length(ns) * length(Ns) * length(deltas) * repeats
runs = 0

for(n in ns){
  for(N in Ns) {
    for(delta in deltas) {
      for(i in 1:repeats) {
        df.intervention <- rbind(df.intervention, brm_mm_test(n, N, delta, "Intervention")) 
        runs <- runs + 1
        print(paste0(runs, ' of ', total, ' complete!'))
      }
    }
  }
}

m <- lm('Saved ~ Intervention', data=d_human, weights=d_human$weights)
true_t <- coef(m)['Intervention']

# Define the unique values of n
unique_n <- unique(df.intervention$n)

# Create an empty list to store plots
plot_list <- list()

# set baseline font size
bsize = 8.5


# Loop through each unique value of n
for (i in 1:length(unique_n)) {
  # Subset the data for the current value of n
  subset_data <- filter(df.intervention, n == unique_n[i])
  
  # Calculate average diff.width.ci for each combination of N and delta
  avg_diff = subset_data %>% 
    group_by(N, delta) %>%
    summarize(avg_diff_width_ci = mean(diff.width.ci))
  
  # Top row: Line plot of avg diff.width.ci vs. N, colored by delta
  plot1 <- ggplot(avg_diff, aes(x = N, y = avg_diff_width_ci, color = factor(delta))) +
    geom_line(size=1) +
    labs(title = paste("n =", unique_n[i]), x = NULL,y=NULL)+
         #, x = "Number of LLM Obs.", y = "E(Change in width of 95% CI)") +
    scale_color_discrete(name = "Delta") +
    theme_minimal(base_size = bsize) +
    theme(legend.position = "bottom")
  
  # Bottom row: Boxplot of est vs. N, colored by delta
  boxp_data <- subset_data %>% 
    group_by(N,delta) %>% 
    summarize(y000 = min(est),
              y0025 = quantile(est,0.025),
              y050 = quantile(est,0.50),
              y0975 = quantile(est,0.975),
              y100 = max(est))
  
  plot2 <- ggplot(boxp_data, aes(x = factor(N), fill = factor(delta))) +
    geom_boxplot(aes(ymin=y000, lower=y005, middle=y050, upper=y095, ymax=y100),
                 stat = "identity") +
    labs(title = NULL, x = NULL, y = NULL)+
    #labs(title = NULL, x = "Number of LLM Obs.", y = "MAP of the Treatment Effect") +
    scale_fill_discrete(name = "Delta") +
    geom_hline(yintercept = true_t, linetype = "dashed") +
    theme_minimal(base_size = bsize) +
    theme(legend.position = "bottom")
  
  # Add plots to the list
  plot_list[[i]] <- plot2

  
  # Add plots to the list
  plot_list[[i]] <- plot1
  plot_list[[i + length(unique_n)]] <- plot2
}


# Arrange plots in a grid and display
plot_list[[1]]  = plot_list[[1]] + labs(y="E(Change in width of 95% CI)")
plot_list[[7]]  = plot_list[[7]] + labs(y="MAP of the Treatment Effect")
for(p in 7:12) plot_list[[p]] = plot_list[[p]] + labs(x="Number of LLM Observations") 

ggarrange(plot_list[[1]],plot_list[[2]],plot_list[[3]],plot_list[[4]],plot_list[[5]],plot_list[[6]],
          plot_list[[7]],plot_list[[8]],plot_list[[9]],plot_list[[10]],plot_list[[11]],plot_list[[12]],
          ncol = 6,nrow=2,common.legend = T,legend = "bottom")
ggsave("MoralMachine/Code/4_Bayes/BayesPlot_Run2.png",bg="white", width = 13.5, height = 6)

# Reset the layout to the default
par(mfrow = c(1, 1))

#write_csv(df.intervention,"MoralMachine/Code/4_Bayes/BayesRM_dfintervention_Run2.csv")
