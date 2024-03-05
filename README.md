# IntegrativeExperimentsGAI

## Moral Machine Experiment

We use the `SharedResponses.csv.tar.gz` data file from Awad et al. (2018) from [osf.io](https://osf.io/3hvt2/?view_only=4bb49492edee4a8eb1758552a362a2cf). This [document](https://osf.io/wt6mc?view_only=4bb49492edee4a8eb1758552a362a2cf) is a useful description of the data structure. 

For our analysis, we use a subset called `SharedResponsesClean.zip`. We kept only observations for which there are two observations representing complete data on a scenario. We also kept responses from the US and removed any other missing values.

The GPT predictions are from Takemoto (2023). The data are downloadable from this [dropbox](https://www.dropbox.com/sh/1y8imez75gz2i92/AAAuOwOz20ohZ8F6A1KF_kiCa?dl=0). 

These two data sources were joined on all covariates except for `Saved` (the dependent variable) and `Intervention` (a feature of the decision). Trying to join on Intervention leads to having no variation in the joined dataset. Possibly I overlooked something here (e.g. the paired data structure: two rows per respondent).

