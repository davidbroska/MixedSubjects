# This script creates the indecices of rows of US respondents for which two rows are available 
library(data.table)

# Read moral machine dataset
SharedResponses = fread("MoralMachine/Data/Awad2018/Data/Moral Machine Data/SharedResponses.csv",
                        select=c("ResponseID","Man","UserCountry3"), key = "ResponseID",
                        colClasses = "character")

SharedResponses = SharedResponses[-which(SharedResponses$Man == "(nan, nan, nan, nan)")]
SharedResponses = SharedResponses[UserCountry3 == "USA"]
SharedResponses = SharedResponses[, -c("Man","UserCountry3")]


SharedResponses = SharedResponses[, CountID := .N, by = ResponseID]
Index = which(SharedResponses$CountID == 2)

SharedResponses = SharedResponses[Index]

fwrite(SharedResponses, "MoralMachine/Data/Temp/SharedResponses2Rows.csv", compress = "gzip")


