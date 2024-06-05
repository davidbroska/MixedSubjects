

# Data collection 

To simulate reponses with the provided code, follow these steps: 

1. Get Python 3.11 and PyTorch via `pip install torch==1.2` 
2. Run `pip install -r 1/DataCollection/requirements.txt`


# Code 

#### `1_Functions.R`

This script contains functions used in the analysis, some of which where adopted from the replication material for the Moral Machine experiment provided by Awad et al. (2018).

#### `2_CreateSampleForPrompting.R`

This script creates as sample of responsdents from the Moral Machine that is representive of the American population on age, education, gender, and income. Based on the 2016 American Community Survey, we calculated the percentage of Americans with combinations of these demographic characteristis (e.g. 35-55 years old, graduate degree, woman, annual income of $50,001-$100,000). We then used the same demographic categories to create a stratified sample of respondents from the Moral Machine Experiment. For each combination of demographic characteristics, we randomly selected a number of respondents approximately equal to the number of respondents we would expect if they were sampled from a population with the demographic makeup of the American Community Survey. The resulting stratified sample is more representative of the American population than the respondents to the Moral Machine experiment (see [Figure]()). We obtained a stratified sample of n=2,097 Americans who evaluated a total of 22,315 moral dilemmas.

#### `3_CalculateCostAPI.R`

This script calculates the approximate costs of prompting OpenAI's models to evaluate the 22,315 moral dilemmas. 

#### `4_PromptLLM.ipynb`

This Python notebook creates prompts to the language models and sends API requests.

#### `5_AggregateAndWeight.R`

This script aggregates the output from the API requests and calculates the weights necessary to calculate the Average Marginal Component Effect (AMCE).

#### `6_AnalysisPPI.ipynb`

This Pyhton notebook computes the point estimates and confidence intervals with PPI and conventional inference.


# Data

#### `2_SurveySample.csv.gz`

This file contains the stratified random sample created with `2_CreateSampleForPrompting.R`.


#### `5_SurveySampleLLM.csv.gz`

This file contains the stratified random sample with the predicted responses from the LLMs, aggregated in `5_AggregateAndWeight.R`.


#### `6_ResultsPPI.csv.gz`

This files stores the results from the data analysis with PPI in `6_AnalysisPPI.ipynb`.