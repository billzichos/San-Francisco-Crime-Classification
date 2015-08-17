setwd("~/GitHub/San-Francisco-Crime-Classification")

#library("dplyr")

# read in the sample submission file
sub <- read.csv("sampleSubmission.csv")

# get list of columns
cols <- names(sub)[2:length(names(sub))]
cols <- gsub("\\.", " ", cols)
cols[8] <- "DRUG/NARCOTIC"
cols[13] <- "FORGERY/COUNTERFEITING"
cols[17] <- "LARCENY/THEFT"
cols[21] <- "NON-CRIMINAL"
cols[23] <- "PORNOGRAPHY/OBSCENE MAT"

# read in the test file
test <- read.csv("test.csv")

# strip out everything but the id column
test <- as.data.frame(test$Id)

# add the list of columns to the test data frame
# fill the new columns with zeros
test[,c(cols)] <- 0

# to create a very basic submission file, set everything to burglary.
test$`LARCENY/THEFT` <- 1

# write out the submission file.
write.csv(test, "bz_submission.csv", row.names = FALSE)

# Manually update the first column label to just say "Id"


table(train$Category, km$cluster)[,1]/table(km$cluster)[1]
cumsum(table(train$Category, km$cluster)[,2]/table(km$cluster)[2])
max(which(cumsum(table(train$Category, km$cluster)[,2]/table(km$cluster)[2]) <= 0.5))