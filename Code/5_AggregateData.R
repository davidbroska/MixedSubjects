# Load functions
source("Code/1_Functions.R")

# Load sample from moral machine data
mms = read_csv("Data/2_SurveySample.csv.gz") %>% 
  mutate(across(everything(), as.character))


############################
# Load files with API output
############################

# LLM predictions for 22,315 responses with persona
predictions_wp = c("4_gpt-4-turbo_wp_20240603.csv.gz","4_gpt-3.5-turbo-0125_wp_20240603.csv.gz","4_gpt-4o_wp_20240603.csv.gz")

# LLM predictions for 5,000 responses without persona
predictions_np = c("4_gpt-4-turbo_np_20240603.csv.gz","4_gpt-3.5-turbo-0125_np_20240603.csv.gz","4_gpt-4o_np_20240603.csv.gz")

# Replicates of 5,000 predictions with persona (identical prompts as in files ending on _wp_20240603.csv.gz)
replicates_wp  = c("4_gpt-4-turbo_wp_20240605.csv.gz","4_gpt-3.5-turbo-0125_wp_20240605.csv.gz","4_gpt-4o_wp_20240605.csv.gz",
                   "4_gpt-4-turbo_wp_20240606.csv.gz","4_gpt-3.5-turbo-0125_wp_20240606.csv.gz","4_gpt-4o_wp_20240606.csv.gz")

files = c(predictions_wp,predictions_np,replicates_wp)

# Aggregate files
df = files %>% 
  lapply(., function(file){
    
    writeLines(file)
    
    # read data
    dd = paste0("Data/",file) %>% 
      read_csv(show_col_types=F) %>% 
      mutate(across(everything(), as.character)) %>% 
      select(ExtendedSessionID:Cat, matches(".Saved"))  
    
    # define unique column names for replicates
    if(str_detect(file,"05.csv.gz")){
      icol = str_which(colnames(dd), ".Saved")
      colnames(dd)[icol] = paste0(colnames(dd)[icol], "_2")
    }
    if(str_detect(file,"06.csv.gz")){
      icol = str_which(colnames(dd), ".Saved")
      colnames(dd)[icol] = paste0(colnames(dd)[icol], "_3")
    }

    return(dd)
    
  }) %>% 
  # join all data sets with predictions
  purrr::reduce(full_join, by=colnames(mms)) %>% 
  full_join(mms, ., by=colnames(mms)) %>% 
  # keep rows with at least one outcome value predicted by LLM
  filter(if_any(matches("p_Saved"), ~!is.na(.)))



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

gpt35turbo0125_wp_reps = c("gpt35turbo0125_wp_Saved", "gpt35turbo0125_wp_Saved_2", "gpt35turbo0125_wp_Saved_3")
gpt4turbo_wp_reps = c("gpt4turbo_wp_Saved", "gpt4turbo_wp_Saved_2", "gpt4turbo_wp_Saved_3")
gpt4o_wp_reps = c("gpt4o_wp_Saved", "gpt4o_wp_Saved_2", "gpt4o_wp_Saved_3")

# find modal prediction
df$gpt35turbo0125_wp_Saved_mode = find_mode(df, gpt35turbo0125_wp_reps)
df$gpt4turbo_wp_Saved_mode = find_mode(df, gpt4turbo_wp_reps)
df$gpt4o_wp_Saved_mode = find_mode(df, gpt4o_wp_reps)


# verify
df %>% select(all_of(c(gpt35turbo0125_wp_reps,"gpt35turbo0125_wp_Saved_mode"))) %>% head()
df %>% select(all_of(c(gpt4turbo_wp_reps,"gpt4turbo_wp_Saved_mode"))) %>% head()
df %>% select(all_of(c(gpt4o_wp_reps,"gpt4o_wp_Saved_mode"))) %>% head()

# Check for NA values
summarize(df,across(matches("wp_Saved"), ~sum(is.na(.))))

# Save file with predictions from LLMs
write_csv(df,"Data/5_SurveySampleLLM.csv.gz")
