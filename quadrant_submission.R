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
str(test)
# strip out everything but the id column
test <- as.data.frame(test[,c(1,6,7)])

# add the list of columns to the test data frame
# fill the new columns with zeros
test[,c(cols)] <- 0

# to create a very basic submission file, set everything to burglary
# except quadrant 3.
test$`OTHER OFFENSES`[test$X>-122.4393 & test$Y<37.76393] <- 1
test$`LARCENY/THEFT`[test$X<=-122.4393 & test$Y>=37.76393] <- 1
test$`LARCENY/THEFT`[test$X>-122.4393 & test$Y>=37.76393] <- 1
test$`LARCENY/THEFT`[test$X<=-122.4393 & test$Y>=37.76393] <- 1

# now we can strip out X and Y
test <- test[,c(1,4:42)]

# write out the submission file.
write.csv(test, "bz_quad_submission.csv", row.names = FALSE)

# Manually update the first column label to just say "Id"