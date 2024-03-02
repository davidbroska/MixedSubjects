library(ggplot2)
library(tidyr)
theme_set(theme_bw(base_size = 10))
df = read_csv("Code/3_result.csv") %>% select(- ...1) %>% 
  mutate(width_ols = abs(upper_CI_ols - lower_CI_ols),
         width_ppi = abs(upper_CI_ppi - lower_CI_ppi), 
         n = as.integer(n), 
         N = as.integer(N))
summarise_if(df, is.numeric, var)
summarise(df, across(c(n,N), list(min=min,max=max,mean=mean,sd=sd)))

text = df %>% filter(n %in% c(250,500,750), N==7500) %>% distinct(var,n,N,width_ols) %>% 
  mutate(width_ols = round(width_ols,2))
df %>% 
  ggplot(aes(N,n, z=width_ppi)) +
  geom_contour_filled() +
  facet_wrap(~ var, ncol = 7) + 
  geom_text(aes(N,n,label=width_ols),data = text,size =2,inherit.aes = F, color="white") + 
  labs(fill = "Width of PPI CI", y = "n of human responses",
       x = "N of GPT4 responses", title="Contour plot of width of CI across sample sizes of human responses and GPT predictions")
ggsave("Figures/3_ContourPlot.png",width=11,height=8)

# df %>% 
#   filter(N %in% c(5000)) %>% 
#   pivot_longer(cols = c(width_ols,width_ppi)) %>% 
#   ggplot(aes(n, value,color = name)) +
#   facet_wrap(~ var, ncol=7)
# ggsave("Figures/3_ContourPlot.png",width=6,height=8)
