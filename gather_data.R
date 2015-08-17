wd <- "~/GitHub/San-Francisco-Crime-Classification"

setwd(wd)

# The following files are provided
#   - test.csv.zip
#   - train.csv.zip
#   - sampleSubmission.csv.zip

source("~/GitHub/Get-Raw-Data/download.R")
downloadSingleKaggleZip("sf-crime","train.csv.zip", "train.csv")
downloadSingleKaggleZip("sf-crime","test.csv.zip", "test.csv")
downloadSingleKaggleZip("sf-crime","sampleSubmission.csv.zip", "sampleSubmission.csv")
