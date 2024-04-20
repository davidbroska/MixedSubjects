library(dplyr)
library(latex2exp)
library(ggplot2)
theme_set(theme_bw())
tab = tibble(k = 2:100) %>% 
  mutate(r = 1/k + 0.5)
ggplot(tab,aes(k,r)) + 
  geom_line() + 
  ylim(0,NA) +
  labs(x=TeX("Ratio of LLM to human responses N/n"),y=TeX("Correlation $r$"),
       title=TeX("Conditions for PPI to be more precise than classic inference"), 
       subtitle = TeX("Minimum correlation between $Y$ and $Y^{S(t)}$ and ratio of LLM to human responses")) +
  theme_bw(base_size = 8)
ggsave("PPI/minimumr.png", width=5,height=5)



# ratio of PPI to classic CI 
ratioPPIclassic = function(lambda,N_over_n,r){
  sqrt(lambda*(1/N_over_n) + 1 + lambda - 2 * r * sqrt(lambda))
}

p(1,10,0.5)


# N over n ratio 
x <- seq(1, 20)
# Generate 100 y values for each x value
y <- rep(seq(0.01, 1, length.out = 100), times = length(x))

# Repeat each x value 100 times
x <- rep(x, each = 100)

# Combine x and y into a data frame
df = tibble(N_over_n = x, r = y) %>% 
  mutate(lambda = 0.5) %>% 
  mutate(ratio = ratioPPIclassic(lambda,N_over_n,r))

ggplot(df,aes(x=N_over_n,y=r,z=ratio)) +
  geom_contour_filled(breaks = seq(0,1.5,by = 0.25)) +
  geom_hline(yintercept = 0.5,color="white",size=.1) + 
  scale_x_continuous(breaks = c(1,5,10,15,20)) +
  labs(x = "Ratio of LLM to human responses N/n", 
       y = TeX("Correlation $r$"), 
       fill = "Ratio of PPI CI width\nto classical CI width") 
ggsave("PPI/PPIlambda1.png", width=7,height=5)

# faceted plot
dd = df %>% 
  bind_rows(mutate(df,lambda=1)) %>% 
  bind_rows(mutate(df,lambda=1.5)) %>% 
  mutate(ratio = p(lambda,N_over_n,r))

appender <- function(string) TeX(paste("$\\lambda = $", string))

ggplot(dd,aes(x=N_over_n,y=r,z=ratio)) +
  geom_contour_filled(breaks = c(0,0.5,1,1.5,2)) +
  geom_hline(yintercept = 0.5,color="white",size=.1) + 
  scale_x_continuous(breaks = c(1,5,10,15,20)) +
  labs(x = TeX("Ratio of LLM to human responses $N/n$"), 
       y = TeX("Correlation $r$"), 
       fill = "Ratio of PPI CI width\nto classical CI width") +
  facet_wrap(~ lambda, labeller = as_labeller(appender, default = label_parsed))
ggsave("PPI/PPIlambda05115.png", width=9,height=5)



# ------------------------------------------------------------------------------
# CI Width across lambdas and r
dd = tibble()
ns = seq(100,1000,50)
Ns = seq(100,2000,50)
lambdas = c(0.5,1,1.5)
rs = c(0.25,0.5,0.75)

# looloolooloop
for (l in seq_along(lambdas)) {
  for (r in seq_along(rs)) {
    for(n in seq_along(ns)) {
      for(N in seq_along(Ns)){
        temp = tibble(lambda=lambdas[l], r=rs[r], n=ns[n], N=Ns[N])
        dd = bind_rows(dd,temp)
      }
    }
  }
}
dd %>% 
  mutate(N_over_n = N/n, 
         ratio = ratioPPIclassic(lambda,N_over_n,r)) %>% 
  ggplot(aes(x=N,y=n,z=ratio)) + 
  geom_contour_filled(breaks = c(0,0.5,1,1.5,2,10)) +
  coord_fixed(ratio = 1) +
  scale_y_continuous(breaks = c(500,1000)) +
  facet_wrap(~r + lambda, 
             labeller = label_bquote(lambda~"="~.(lambda)~","~r~"="~.(r))) + 
  labs(x = TeX("Number of LLM simulations $N$"), 
       y = TeX("Number of human subjects $n$"),
       title = TeX("Ratio of confidence intervals at values of $\\lambda$ and $\\textit{r}$"), 
       fill = "Ratio of PPI CI width\nto classical CI width") 
ggsave("PPI/RatiosNn.png", width=6.5,height=6)

# ------------------------------------------------------------------------------
# Variance


varPPI = function(sigmaY,lambda,N,n,r){
  sigmaY^2* (lambda/N + 1/n+ lambda/n - (2*r*sqrt(lambda)) / n)
  
}


dd = dd %>% 
  mutate(sigmaY = 1,
         varPPI = varPPI(sigmaY=sigmaY,lambda=lambda,N=N,n=n,r=r), 
         varClassic = sigmaY^2 / n, 
         ratio = varPPI / varClassic) %>% 
  filter(varPPI > 0)
dd
ggplot(dd,aes(x=N,y=n,z=varPPI)) + 
  geom_contour_filled() +
  coord_fixed(ratio = 1)+
  facet_wrap(~r + lambda, 
             labeller = label_bquote(lambda~"="~.(lambda)~","~r~"="~.(r))) + 
  labs(title = TeX("Indifference curve for n and N at values of $\\lambda$ and $\\textit{r}$"), 
       fill = "Variance of\nPPI estimate") 

ggplot(dd,aes(x=N,y=n,z=ratio)) + 
  geom_contour_filled(breaks = c(.2,.4,.6,.8,1,2,5,20)) +
  coord_fixed(ratio = 1)+
  facet_wrap(~r + lambda, 
             labeller = label_bquote(lambda~"="~.(lambda)~","~r~"="~.(r))) + 
  labs(title = TeX("Indifference curve for n and N at values of $\\lambda$ and $\\textit{r}$"), 
       fill = "Variance of\nPPI estimate"
       ) 

ggsave("PPI/PPIindifference.png", width=6.5,height=6)
