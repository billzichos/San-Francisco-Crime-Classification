setwd("~/GitHub/San-Francisco-Crime-Classification")

# read in the training file and prepare for combining with test.
train <- read.csv("train.csv")
train$Source <- "Train"
train$Id <- 0

# read in the test file and prepare for combining with train.
test <- read.csv("test.csv")
test$Source <- "Test"
test$Category <- ""
test$Descript <- ""
test$Resolution <- ""

# combine the data files for feature generation
library("dplyr")
train <- select(train, Id, Dates, Category, Descript, DayOfWeek, PdDistrict, Resolution, Address, X, Y, Source)
test <- select(test, Id, Dates, Category, Descript, DayOfWeek, PdDistrict, Resolution, Address, X, Y, Source)

        df <- rbind(train, test)

# add variables that are needed for submission
sub <- read.csv("sampleSubmission.csv")
cols <- names(sub)[2:length(names(sub))]
cols <- gsub("\\.", " ", cols)
cols[8] <- "DRUG/NARCOTIC"
cols[13] <- "FORGERY/COUNTERFEITING"
cols[17] <- "LARCENY/THEFT"
cols[21] <- "NON-CRIMINAL"
cols[23] <- "PORNOGRAPHY/OBSCENE MAT"
df[,c(cols)] <- 0


# derive some date/time-related variables
library("lubridate")
df$Dates.Hour <- hour(df$Dates)
df$Dates.Quarter <- quarter(df$Dates)
df$Dates.Month <- month(df$Dates)
df$Dates.Wkdy <- wday(df$Dates)
df$Dates.LogNumeric <- log10(as.numeric(as.POSIXct(df$Dates)))

# derive some PD District-related variables
df$PdDistrict.NORTHERN.flag <- 0
df$PdDistrict.PARK.flag <- 0
df$PdDistrict.INGLESIDE.flag <- 0
df$PdDistrict.BAYVIEW.flag <- 0
df$PdDistrict.RICHMOND.flag <- 0
df$PdDistrict.CENTRAL.flag <- 0
df$PdDistrict.TARAVAL.flag <- 0
df$PdDistrict.TENDERLOIN.flag <- 0
df$PdDistrict.MISSION.flag <- 0
df$PdDistrict.SOUTHERN.flag <- 0

df[which(df$PdDistrict=="NORTHERN"),which(names(df)=="PdDistrict.NORTHERN.flag")] <- 1
df[which(df$PdDistrict=="PARK"),which(names(df)=="PdDistrict.PARK.flag")] <- 1
df[which(df$PdDistrict=="INGLESIDE"),which(names(df)=="PdDistrict.INGLESIDE.flag")] <- 1
df[which(df$PdDistrict=="BAYVIEW"),which(names(df)=="PdDistrict.BAYVIEW.flag")] <- 1
df[which(df$PdDistrict=="RICHMOND"),which(names(df)=="PdDistrict.RICHMOND.flag")] <- 1
df[which(df$PdDistrict=="CENTRAL"),which(names(df)=="PdDistrict.CENTRAL.flag")] <- 1
df[which(df$PdDistrict=="TARAVAL"),which(names(df)=="PdDistrict.TARAVAL.flag")] <- 1
df[which(df$PdDistrict=="TENDERLOIN"),which(names(df)=="PdDistrict.TENDERLOIN.flag")] <- 1
df[which(df$PdDistrict=="MISSION"),which(names(df)=="PdDistrict.MISSION.flag")] <- 1
df[which(df$PdDistrict=="SOUTHERN"),which(names(df)=="PdDistrict.SOUTHERN.flag")] <- 1

# derive some address-related features
# if intersection:
df$Address.Intersection.Street1 <- ""
df$Address.Intersection.Street2 <- ""
        # split the string on " " - strsplit("1500 Block of OAK ST", " ")
        # remove "/", "ST", "AV" anything else?
        # confirm only 2 values remain
        # order the values alphabetically
        # capture first alpha ordered street in df$Address.Intersection.Street1
        # capture 2nd alpha ordered street in df$Address.Intersection.Street2
# if block:
df$Address.Block.Number <- 0
df$Address.Block.Street < ""
        # capture block number in df$Address.Block.Number
        # capture street name in df$Address.Block.Street
# intersections - OAK ST / LAGUNA ST
grep(" / ", df$Address)
# blocks - 1500 Block of ####
grep("Block of", df$Address)
grep("^[0:9]{1,6}", df$Address)
strsplit(df$Address, " ")
# Does AVE vs. ST mean anything like it does in NY - NS vs. EW?
# The numbered avenues run N/S and make up the Sunset District on the West Side.
# Does the ordering of streets for intersections have any meaning?

# block
# street number
# street name 1
# street name 2
# street type

# derive some lat/long-related features
# hemisphere
df$X.WesternHemisphere.Flag <- 0
df$Y.NorthernHemisphere.Flag <- 0
# degrees
df$X.degrees <- as.character(abs(df$X))
df$Y.degrees <- 0
# minutes
# seconds
# fraction

train.temporal.features$Dates <- NULL
train.temporal.features$Descript <- NULL
train.temporal.features$DayOfWeek <- NULL
train.temporal.features$PdDistrict <- NULL
train.temporal.features$Resolution <- NULL
train.temporal.features$Address <- NULL
train.temporal.features$X <- NULL
train.temporal.features$Y <- NULL

km <- kmeans(train.temporal.features, 36, iter.max = 10)
table(train$Category, km$cluster)
train$temporal.kmean.cluster <- km$cluster

#head(train)
#str(train)
str(train.temporal.features)
#summary(train)
#table(train$Category)
#plot(train$Category)

#What is the most common crime in San Francisco?
which.max(summary(train$Category))

plot(train$Category)

#What is the middle Latitude? Longitude?
#Use the mean lat/longs to divide the city into quadrants.
#What is the most common crime by quadrant?
mean(train$X)
mean(train$Y)
plot(train$Y)
with(train, plot(X,Y))
trainClean <- train[which(train$Y!=90),]

with(trainClean, plot(X,Y))

vLine <- mean(trainClean$X)
hLine <- mean(trainClean$Y)

library("ggplot2")
g <- ggplot(trainClean, aes(X,Y))
g + geom_point(aes(color=Category)) + 
        geom_hline(aes(yintercept = hLine)) +
        geom_vline(aes(xintercept = vLine))

trainClean$Quadrant <- 1
trainClean$Quadrant[trainClean$Y>=hLine & trainClean$X>vLine] <- 2
trainClean$Quadrant[trainClean$Y<hLine & trainClean$X>vLine] <- 3
trainClean$Quadrant[trainClean$Y<hLine & trainClean$X<=vLine] <- 4
summary(trainClean$Quadrant)

#What is the most common crime for each Quadrant?
which.max(summary(trainClean$Category[trainClean$Quadrant==1]))
which.max(summary(trainClean$Category[trainClean$Quadrant==2]))
which.max(summary(trainClean$Category[trainClean$Quadrant==3]))
which.max(summary(trainClean$Category[trainClean$Quadrant==4]))


# To prove the point that some crimes are very isolated.
g <- ggplot(trainClean[which(trainClean$Category=="PROSTITUTION"),], aes(X,Y))
g + geom_point(aes(color=Category)) + 
        geom_hline(aes(yintercept = hLine)) +
        geom_vline(aes(xintercept = vLine))

g <- ggplot(trainClean[which(trainClean$Category=="LOITERING"),], aes(X,Y))
g + geom_point(aes(color=Category)) + 
        geom_hline(aes(yintercept = hLine)) +
        geom_vline(aes(xintercept = vLine))

#Let's look for other crimes that are heavily dependant on location.
g <- ggplot(trainClean, aes(X,Y))
g + geom_point(alpha = 1/20) + facet_wrap( ~ Category, nrow = 8, ncol = 5)

#What was the deal with those crimes on the 90th degree of latitude?
length(train[train$Y==90,2])
summary(train[train$Y==90,2])
which.max(summary(train[train$Y==90,2]))

#Dig into the hour of the day.
hours <- 0:23
findMax <- function(x) {
        which.max(summary(trainClean$Category[trainClean$Hour==x]))
}
sapply(hours, findMax)

g <- ggplot(trainClean[trainClean$Hour==12 & trainClean$Category!='LARCENY/THEFT',], aes(X,Y))
g + geom_point(alpha = 1/5) + facet_wrap( ~ Category, nrow = 8, ncol = 5)

# Let's look at how critical hour of day
g <- ggplot(trainClean[trainClean$Category=='ARSON',], aes(Hour))
g + geom_bar() + facet_grid(Category ~ .)


plot(trainClean[,c(10:14)], col = cl$cluster)


# add some date/time-related features
library("lubridate")
train.temporal.features <- train
train.temporal.features$Hour <- hour(train$Dates)
train.temporal.features$Quarter <- quarter(train$Dates)
train.temporal.features$Month <- month(train$Dates)
train.temporal.features$Wkdy <- wday(train$Dates)
train.temporal.features$Numeric <- log10(as.numeric(as.POSIXct(train$Dates)))
train.temporal.features$Category <- NULL
train.temporal.features$Dates <- NULL
train.temporal.features$Descript <- NULL
train.temporal.features$DayOfWeek <- NULL
train.temporal.features$PdDistrict <- NULL
train.temporal.features$Resolution <- NULL
train.temporal.features$Address <- NULL
train.temporal.features$X <- NULL
train.temporal.features$Y <- NULL