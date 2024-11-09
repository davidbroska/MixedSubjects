# Load functions
source("Code/1_Functions.R")

# Define custom colors and line types
custom_colors = c("Silicon Sampling" = "#1f78b4","Prediction-Powered Inference" = "#33a02c")

custom_shapes = c("Prediction-Powered Inference" = 16, "Silicon Sampling" = 15)

# Set parameters for plot
linewidth = 0.3
pointsize = 1.3
ymax = 0.1435


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
  ggplot(aes(x = N, y = value, color = method, shape = method)) +
  geom_line(linewidth = linewidth) +
  geom_point(size = pointsize) +
  labs(
    title = "Bias",
    x = "N",
    y = "Bias"
  ) +
  scale_color_manual(values = custom_colors) +
  scale_shape_manual(values = custom_shapes) +
  scale_y_continuous(breaks = seq(0,1,0.02), limits = c(NA, ymax))


# Create Coverage Plot
coverage_plot_n = dfn_long %>%
  filter(metric == "coverage") %>%
  ggplot(aes(x = N, y = value, color = method, shape = method)) +
  geom_hline(yintercept = 0.95, color = "darkgrey", linetype = "dashed") +
  geom_line(linewidth = linewidth) +
  geom_point(size = pointsize) +
  labs(
    title = "Coverage",
    x = "N",
    y = "Coverage"
  ) +
  scale_color_manual(values = custom_colors) +
  scale_shape_manual(values = custom_shapes) +
  scale_y_continuous(labels = percent_format(accuracy = 1), breaks = seq(0,1,0.2), limits = c(0,1)) 


# Create Standard Error Plot
se_plot_n = dfn_long %>%
  filter(metric == "se") %>%
  ggplot(aes(x = N, y = value, color = method, shape = method)) +
  geom_line(linewidth = linewidth) +
  geom_point(size = pointsize) +
  labs(
    title = "Standard Error",
    x = "N",
    y = "Standard Error"
  ) +
  scale_color_manual(values = custom_colors) +
  scale_shape_manual(values = custom_shapes) +
  scale_y_continuous(breaks = seq(0,1,0.02), limits = c(0, ymax)) 


# Create Root Mean Squared Error Plot
rmse_plot_n = dfn_long %>%
  filter(metric == "rmse") %>%
  ggplot(aes(x = N, y = value, color = method, shape = method)) +
  geom_line(linewidth = linewidth) +
  geom_point(size = pointsize) +
  labs(
    title = "Root Mean Square Error",
    x = "N",
    y = "RMSE"
  ) +
  scale_color_manual(values = custom_colors) +
  scale_shape_manual(values = custom_shapes) +
  scale_y_continuous(breaks = seq(0,1,0.02), limits = c(0, ymax)) 





######################
# Create combined plot 
######################

# Combine the plots side by side with a shared legend at the bottom
combined_plot = bias_plot_n + se_plot_n + rmse_plot_n + coverage_plot_n +
  plot_layout(ncol = 2, guides = "collect") &
  plot_annotation(tag_levels = 'a') & 
  guides(color = guide_legend(keywidth = 2)) & 
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.tag.position = c(.08, 1),
    plot.tag = element_text(face = 'bold')
  ) + theme(plot.margin = unit(c(0.12, 0.2, -0.15, 0.06), "cm")) 

# Display the combined plot
combined_plot

# Save to pdf
ggsave(filename = paste0("Figures/simulation_study.pdf"), 
       plot=combined_plot, width=7, height=6)




