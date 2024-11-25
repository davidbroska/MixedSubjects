# Load functions
source("Code/1_Functions.R")

# Load sample from moral machine data
mms = read_csv("Data/2_SurveySample.csv.gz") %>% 
  mutate(across(everything(), as.character))


############################
# Load files with API output
############################

# Read files from data folder
files = list.files("Data/",pattern = "^4_.*[0-9]{8}[.]csv[.]gz$")
files = list.files("Data/",pattern = "^4_.*[0-9]{8}_[0-9]r[.]csv[.]gz$")

files = c("4_gpt4turbo_wp_20241118.csv.gz",
          "4_gpt35turbo0125_wp_20240603.csv.gz",
          "4_gpt4o_wp_20240603.csv.gz",
          "4_claude35sonnet20241022_wp_20241115.csv.gz")

# Assign number to prediction if LLM was prompted multiple times
files_df = tibble(files) %>% 
  mutate(model = str_remove(files, "_[0-9]+(_[0-9]r)?.csv.gz"),
         date =  str_extract(files,"_[0-9]+(_[0-9]r)?.csv.gz") %>% str_remove_all("[._a-z]+")) %>% 
  group_by(model) %>% 
  arrange(model,date) %>% 
  mutate(nth_prediction = 1:n()) %>% 
  ungroup()
files_df

# Aggregate files
df_list = list()
for(r in 1:nrow(files_df)){
  
  # Get file name
  file = files_df$files[r]
  
  writeLines(file)
  
  # Read data frame containing the predictions
  dd = paste0("Data/",file) %>% 
    read_csv(show_col_types=F) %>% 
    mutate(across(everything(), as.character)) %>% 
    select(ExtendedSessionID:Cat, matches(".Saved")) 
  
  # nth prediction by LLM
  nth = files_df$nth_prediction[r]
  
  # Disambiguate column names by counting the nth prediction
  icol = str_which(colnames(dd), ".Saved")
  colnames(dd)[icol] = paste0(colnames(dd)[icol], "_", nth)
  
  # Append data frame to list
  df_list[[r]] = dd
  
}


df = df_list %>% 
  # join all data sets with predictions
  purrr::reduce(full_join, by=colnames(mms)) %>% 
  full_join(mms, ., by=colnames(mms)) %>% 
  # keep rows with at least one outcome value predicted by LLM
  filter(if_any(matches("p_Saved_"), ~!is.na(.)))



##############################
# Calculate mode of replicates
##############################
find_mode = function(.data, .vars, .na_rm=F){
  
  # subset data to relevant column
  df = .data[,.vars]
    
  # find mode per row for these columns
  apply(df, MARGIN=1, FUN=function(row){
    
    # get counts
    counts = row %>%
      table(useNA = "always") %>%
      sort(., decreasing =T)
    
    # find mode
    mode = counts %>%
      names() %>%
      as.integer() %>%
      {{.[1]}}
    
    # rename 
    names(counts)[is.na(names(counts))] = "<NA>"
    
    # set mode to NA if at least one value is NA
    if(counts["<NA>"] >= 1 & .na_rm==F)  mode = NA
    
    return(mode)
  })
}



# # find modal prediction for GPT 3.5 Turbo with persona 
# gpt35turbo0125_wp_reps = str_subset(colnames(df), "gpt35turbo0125_wp")
# df$gpt35turbo0125_wp_Saved_mode = find_mode(df, gpt35turbo0125_wp_reps)
# 
# # find modal prediction for GPT4 Turbo with persona
# gpt4turbo_wp_reps = str_subset(colnames(df), "gpt4turbo_wp")
# df$gpt4turbo_wp_Saved_mode = find_mode(df, gpt4turbo_wp_reps)
# 
# # find modal prediction for GPT4o with persona
# gpt4o_wp_reps = str_subset(colnames(df), "gpt4o_wp")
# df$gpt4o_wp_Saved_mode = find_mode(df, gpt4o_wp_reps)
# 
# # find modal prediction across all models with persona
# all_wp_reps = c(gpt35turbo0125_wp_reps,gpt4turbo_wp_reps,gpt4o_wp_reps)
# df$all_wp_Saved_mode = find_mode(df, all_wp_reps)
# 
# # verify calculation of mode
# df %>% select(all_of(c(gpt35turbo0125_wp_reps,"gpt35turbo0125_wp_Saved_mode"))) %>% head()
# df %>% select(all_of(c(gpt4turbo_wp_reps,"gpt4turbo_wp_Saved_mode"))) %>% head()
# df %>% select(all_of(c(gpt4o_wp_reps,"gpt4o_wp_Saved_mode"))) %>% head()

# Check for NA values
summarize(df,across(matches("wp_Saved"), ~sum(is.na(.)))) %>% 
  pivot_longer(cols = everything(), names_to="model", values_to="NA_count")

# Save file with predictions from LLMs
write_csv(df,"Data/5_SurveySampleLLM.csv.gz")


