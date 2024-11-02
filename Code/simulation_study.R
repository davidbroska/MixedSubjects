# Load functions
source("Code/1_Functions.R")

# Define custom colors and line types
custom_colors = c("Prediction-Powered Inference" = "#F21A00",
                  "Silicon Sampling" = "#3B9AB2")

custom_linetypes = c("Prediction-Powered Inference" = "solid",
                     "Silicon Sampling" = "dashed")

# Set parameters for plot
linewidth = 0.3
pointsize = 0.8
ymax = 0.14


##############################
# Figures with N on the x-axis
##############################

# Import averaged simulation results
dfn = read_csv("Data/simulation_study_N.csv.gz")

# Transform data to long format and update method labels
dfn_long = dfn %>%
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


# Create Bias Plot
bias_plot_n = dfn_long %>%
  filter(metric == "bias") %>%
  ggplot(aes(x = N, y = value, color = method, linetype = method)) +
  geom_line(linewidth = linewidth) +
  geom_point(size = 1) +
  labs(
    title = "Bias",
    x = "N",
    y = "Bias"
  ) +
  scale_color_manual(values = custom_colors) +
  scale_linetype_manual(values = custom_linetypes, guide = "none") +
  scale_y_continuous(breaks = seq(0,1,0.02), limits = c(NA, ymax))


# Create Coverage Plot
coverage_plot_n = dfn_long %>%
  filter(metric == "coverage") %>%
  ggplot(aes(x = N, y = value, color = method, linetype = method)) +
  geom_hline(yintercept = 0.95, color = "darkgrey", linetype = "dashed") +
  geom_line(linewidth = linewidth) +
  geom_point(size = 1) +
  labs(
    title = "Coverage",
    x = "N",
    y = "Coverage"
  ) +
  scale_color_manual(values = custom_colors) +
  scale_linetype_manual(values = custom_linetypes, guide = "none") +
  scale_y_continuous(labels = percent_format(accuracy = 1), breaks = seq(0,1,0.2), limits = c(0,1)) 


# Create Standard Error Plot
se_plot_n = dfn_long %>%
  filter(metric == "se") %>%
  ggplot(aes(x = N, y = value, color = method, linetype = method)) +
  geom_line(linewidth = linewidth) +
  geom_point(size = 1) +
  labs(
    title = "Standard Error",
    x = "N",
    y = "Standard Error"
  ) +
  scale_color_manual(values = custom_colors) +
  scale_linetype_manual(values = custom_linetypes, guide = "none") +
  scale_y_continuous(breaks = seq(0,1,0.02), limits = c(0, ymax)) 


# Create Root Mean Squared Error Plot
rmse_plot_n = dfn_long %>%
  filter(metric == "rmse") %>%
  ggplot(aes(x = N, y = value, color = method, linetype = method)) +
  geom_line(linewidth = linewidth) +
  geom_point(size = 1) +
  labs(
    title = "Root Mean Square Error",
    x = "N",
    y = "RMSE"
  ) +
  scale_color_manual(values = custom_colors) +
  scale_linetype_manual(values = custom_linetypes, guide = "none") +
  scale_y_continuous(breaks = seq(0,1,0.02), limits = c(0, ymax)) 



############################################
# Figures with PPI correlation on the x-axis
############################################

# Import averaged simulation results
dfr = read_csv("Data/simulation_study_rho.csv.gz")

# Transform data to long format and update method labels
dfr_long = dfr %>%
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


# Create Bias Plot
bias_plot_r = dfr_long %>%
  filter(metric == "bias") %>%
  ggplot(aes(x = ppi_corr, y = value, color = method, linetype = method)) +
  geom_line(linewidth = linewidth) +
  geom_point(size = 1) +
  labs(
    title = " ",
    x = bquote("PPI correlation"~tilde(rho)),
    y = "Bias"
  ) +
  scale_color_manual(values = custom_colors) +
  scale_linetype_manual(values = custom_linetypes, guide = "none") +
  scale_x_continuous(breaks = seq(0,1,0.2), limits = c(NA, 1)) + 
  scale_y_continuous(breaks = seq(0,1,0.02), limits = c(NA, ymax))


# Create Coverage Plot
coverage_plot_r = dfr_long %>%
  filter(metric == "coverage") %>%
  ggplot(aes(x = ppi_corr, y = value, color = method, linetype = method)) +
  geom_hline(yintercept = 0.95, color = "darkgrey", linetype = "dashed") +
  geom_line(linewidth = linewidth) +
  geom_point(size = 1) +
  labs(
    title = " ",
    x = bquote("PPI correlation"~tilde(rho)),
    y = "Coverage"
  ) +
  scale_color_manual(values = custom_colors) +
  scale_linetype_manual(values = custom_linetypes, guide = "none") +
  scale_x_continuous(breaks = seq(0,1,0.2), limits = c(NA, 1)) + 
  scale_y_continuous(labels = percent_format(accuracy = 1), breaks = seq(0,1,0.2), limits = c(0,1)) 


# Create Standard Error Plot
se_plot_r = dfr_long %>%
  filter(metric == "se") %>%
  ggplot(aes(x = ppi_corr, y = value, color = method, linetype = method)) +
  geom_line(linewidth = linewidth) +
  geom_point(size = 1) +
  labs(
    title = " ",
    x = bquote("PPI correlation"~tilde(rho)),
    y = "Standard Error"
  ) +
  scale_color_manual(values = custom_colors) +
  scale_linetype_manual(values = custom_linetypes, guide = "none") +
  scale_x_continuous(breaks = seq(0,1,0.2), limits = c(NA, 1)) + 
  scale_y_continuous(breaks = seq(0,1,0.02), limits = c(0, ymax)) 


# Create Root Mean Squared Error Plot
rmse_plot_r = dfr_long %>%
  filter(metric == "rmse") %>%
  ggplot(aes(x = ppi_corr, y = value, color = method, linetype = method)) +
  geom_line(linewidth = linewidth) +
  geom_point(size = 1) +
  labs(
    title = " ",
    x = bquote("PPI correlation"~tilde(rho)),
    y = "RMSE"
  ) +
  scale_color_manual(values = custom_colors) +
  scale_linetype_manual(values = custom_linetypes, guide = "none") +
  scale_x_continuous(breaks = seq(0,1,0.2), limits = c(NA, 1)) + 
  scale_y_continuous(breaks = seq(0,1,0.02), limits = c(0, ymax)) 



######################
# Create combined plot 
######################

# Combine the plots side by side with a shared legend at the bottom
combined_plot = bias_plot_n + se_plot_n + coverage_plot_n + rmse_plot_n +
                bias_plot_r + se_plot_r + coverage_plot_r + rmse_plot_r +
  plot_layout(ncol = 4, guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  )

# Display the combined plot
combined_plot

# Save to pdf
ggsave(filename = paste0("Figures/simulation_study.pdf"), 
       plot=combined_plot, width=12, height=8)




