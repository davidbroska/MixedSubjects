

# Code 

`1_Functions.R` This script contains functions used in the analysis, some of which where adopted from the replication material for the Moral Machine experiment provided by Awad et al. (2018).

`2_CreateSampleForPrompting.R`: This script creates as a sample of respondents from the Moral Machine representative of the American population on age, education, gender, and income. Based on the 2016 American Community Survey, we calculated the percentage of Americans with combinations of these demographic characteristics (e.g. 35-55 years old, graduate degree, woman, annual income of $50,001-$100,000). We then used the same demographic categories to create a stratified sample of respondents from the Moral Machine Experiment. For each combination of demographic characteristics, we randomly selected a number of respondents approximately equal to the number of respondents we would expect if they were sampled from a population with the demographic makeup of the American Community Survey. The resulting stratified sample is more representative of the American population than the respondents to the Moral Machine experiment. We obtained a stratified sample of n=2,097 Americans who evaluated a total of 22,315 moral dilemmas.

`3_CalculateCostAPI.R`: This R script calculates the approximate costs of prompting OpenAI's models to evaluate the 22,315 moral dilemmas. 

`4_PromptLLM.ipynb`:This Python notebook creates prompts to the language models and sends API requests.

`5_AggregateData.R`: This R script aggregates the output from the API requests.

`6_AnalysisPPI.ipynb`: This Python notebook computes the point estimates and confidence intervals with PPI and conventional inference.

`7_CalculateAMCE.R`: This R script calculates the Average Marginal Component Effect (AMCE) with functions adopted from Awad et al. (2018). It also plots the results from the simulation study in `6_AnalysisPPI.ipynb`.


# Data

`2_SurveySample.csv.gz`: This file contains the stratified random sample created with `2_CreateSampleForPrompting.R`.

 `2_usa_00004_ACS_description.pdf`: This document describes the data extract created from the American Community Survey (ACS) on the IPUMS website.

Each of the following datasets contain 22,315 predicted decisions with a prompt that included the persona of the survey respondent.

- `4_gpt-3.5-turbo-0125_wp_20240603.csv.gz`
- `4_gpt-4-turbo_wp_20240603.csv.gz`
- `4_gpt-4o_wp_20240603.csv.gz`

Each of the following datasets contain 5,000 predictions with a prompt including the the persona of the survey respondent. These predictions are meant to be replicates of the predictions made by the language model for the same respondents (see files above with the timestamp `20240503`). These replicates were created assess how consistent the language models are in their predictions and whether a composite score from three predictions would increase the correlation with the observed outcome.

- `4_gpt-3.5-turbo-0125_wp_20240605.csv.gz`
- `4_gpt-3.5-turbo-0125_wp_20240606.csv.gz`
- `4_gpt-4-turbo_wp_20240605.csv.gz`
- `4_gpt-4-turbo_wp_20240606.csv.gz`

The following datasets also contain 5,000 predictions but the prompts did not describe the persona of the survey respondent. These datasets were created to assess whether the correlation if the language model would be given additional context with demographic characteristics.

- `4_gpt-3.5-turbo-0125_np_20240603.csv.gz`
- `4_gpt-4-turbo_np_20240603.csv.gz`
- `4_gpt-4o_np_20240603.csv.gz`


`5_SurveySampleLLM.csv.gz`: This file contains the stratified random sample with the predicted responses from the LLMs, aggregated in `5_AggregateAndWeight.R`.


`6_ResultsPPI.csv.gz` This files stores the results from the data analysis with PPI in `6_AnalysisPPI.ipynb`.