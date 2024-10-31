# Load functions
source("Code/1_Functions.R")
library(patchwork)

# Import averaged simulation results
df = read_csv("Data/simulation_study.csv.gz")

# Transform data to long format and update method labels
df_long = df %>%
  pivot_longer(
    cols = c(bias_ppi, bias_sil, coverage_ppi, coverage_sil, se_ppi, se_sil, rmse_ppi, rmse_sil),
    names_to = c("metric", "method"),
    names_sep = "_",
    values_to = "value"
  ) %>%
  mutate(
    method = case_when(
      method == "ppi" ~ "Prediction-Powered Inference",
      method == "sil" ~ "Silicon Sampling",
      TRUE ~ method
    )
  )

# Define custom colors and line types
custom_colors = c("Prediction-Powered Inference" = "darkred",
                  "Silicon Sampling" = "black")

custom_linetypes = c("Prediction-Powered Inference" = "solid",
                     "Silicon Sampling" = "longdash")

# Create Bias Plot
bias_plot = df_long %>%
  filter(metric == "bias") %>%
  ggplot(aes(x = N, y = value, color = method, linetype = method)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Bias",
    x = "N",
    y = "Bias"
  ) +
  scale_color_manual(values = custom_colors) +
  scale_linetype_manual(values = custom_linetypes, guide = "none") 


# Create Coverage Plot
coverage_plot = df_long %>%
  filter(metric == "coverage") %>%
  ggplot(aes(x = N, y = value, color = method, linetype = method)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Coverage",
    x = "N",
    y = "Coverage"
  ) +
  scale_color_manual(values = custom_colors) +
  scale_linetype_manual(values = custom_linetypes, guide = "none") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) 

# Create Standard Error Plot
se_plot = df_long %>%
  filter(metric == "se") %>%
  ggplot(aes(x = N, y = value, color = method, linetype = method)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Standard Error",
    x = "N",
    y = "Standard Error"
  ) +
  scale_color_manual(values = custom_colors) +
  scale_linetype_manual(values = custom_linetypes, guide = "none") 

# Create Root Mean Squared Error Plot
rmse_plot = df_long %>%
  filter(metric == "rmse") %>%
  ggplot(aes(x = N, y = value, color = method, linetype = method)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "RMSE",
    x = "N",
    y = "RMSE"
  ) +
  scale_color_manual(values = custom_colors) +
  scale_linetype_manual(values = custom_linetypes, guide = "none") 

# Combine the plots side by side with a shared legend at the bottom
combined_plot = bias_plot + se_plot + coverage_plot + rmse_plot +
  plot_layout(ncol = 4, guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  )

# Display the combined plot
combined_plot

ggsave(filename = paste0("Figures/simulation_study.pdf"), 
       plot=combined_plot, width=11, height=5)

