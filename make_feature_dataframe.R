
library(dplyr)
library(MASS)
library(stringr)

## set working directory
setwd("/home/mje/Projects/Bochra_klassifikation/Data")

# load csv files
temp = list.files(pattern="*.csv")
for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i], header=FALSE))

group <- factor(c(rep("coma", 5), rep("vs", 5), rep("mcs", 4), rep("control", 15)))


Drt_max.csv$cond <- factor("max")
Drt_max.csv$subid <- 1:29
Drt_max.csv$deviant <- factor("Drt")
Drt_std.csv$cond <- factor("std")
Drt_std.csv$subid <- 1:29
Drt_std.csv$deviant <- factor("Drt")
Drt_time.csv$cond <- factor("time")
Drt_time.csv$subid <- 1:29
Drt_time.csv$deviant <- factor("Drt")

Gap_max.csv$cond <- factor("max")
Gap_max.csv$subid <- 1:29
Gap_max.csv$deviant <- factor("Gap")
Gap_std.csv$cond <- factor("std")
Gap_std.csv$subid <- 1:29
Gap_std.csv$deviant <- factor("Gap")
Gap_time.csv$cond <- factor("time")
Gap_time.csv$subid <- 1:29
Gap_time.csv$deviant <- factor("Gap")

Frq_max.csv$cond <- factor("max")
Frq_max.csv$subid <- 1:29
Frq_max.csv$deviant <- factor("Frq")
Frq_std.csv$cond <- factor("std")
Frq_std.csv$subid <- 1:29
Frq_std.csv$deviant <- factor("Frq")
Frq_time.csv$cond <- factor("time")
Frq_time.csv$subid <- 1:29
Frq_time.csv$deviant <- factor("Frq")

Int_max.csv$cond <- factor("max")
Int_max.csv$subid <- 1:29
Int_max.csv$deviant <- factor("Int")
Int_std.csv$cond <- factor("std")
Int_std.csv$subid <- 1:29
Int_std.csv$deviant <- factor("Int")
Int_time.csv$cond <- factor("time")
Int_time.csv$subid <- 1:29
Int_time.csv$deviant <- factor("Int")

Spc_max.csv$cond <- factor("max")
Spc_max.csv$subid <- 1:29
Spc_max.csv$deviant <- factor("Spc")
Spc_std.csv$cond <- factor("std")
Spc_std.csv$subid <- 1:29
Spc_std.csv$deviant <- factor("Spc")
Spc_time.csv$cond <- factor("time")
Spc_time.csv$subid <- 1:29
Spc_time.csv$deviant <- factor("Spc")

Std_max.csv$cond <- factor("max")
Std_max.csv$subid <- 1:29
Std_max.csv$deviant <- factor("std")
Std_std.csv$cond <- factor("std")
Std_std.csv$subid <- 1:29
Std_std.csv$deviant <- factor("std")
Std_time.csv$cond <- factor("time")
Std_time.csv$subid <- 1:29
Std_time.csv$deviant <- factor("std")

## make data.frame
data <- rbind(Drt_max.csv, Drt_std.csv, Drt_time.csv,
                Gap_max.csv, Gap_std.csv, Gap_time.csv,
                Frq_max.csv, Frq_std.csv, Frq_time.csv,
                Int_max.csv, Int_std.csv, Int_time.csv,
                Spc_max.csv, Spc_std.csv, Spc_time.csv,
                Std_max.csv, Std_std.csv, Std_time.csv)
names(data) <- c("value", "condition", "subId", "deviant")
data$group <- rep(group, 18)
data[data$subId == 2,] <- NA
data <- data[complete.cases(data),]

group <- factor(c(rep("coma", 4), rep("vs", 5), rep("mcs", 4), rep("control", 15)))

groupData <- group_by(data, subId)
meanData <- summarise(groupData, mean(value))
meanData$group <- group
meanData <- as.data.frame(meanData)
names(meanData) <- c("subId", "value", "group")

maxData <- filter(groupData, condition == "max")
maxDataMean <- summarise(maxData, mean(value))
maxDataMean$group <- group
maxDataMean <- as.data.frame(maxDataMean)
names(maxDataMean) <- c("subId", "value", "group")


stdData <- filter(groupData, condition == "std")
stdDataMean <- summarise(stdData, mean(value))
stdDataMean$group <- group
stdDataMean <- as.data.frame(stdDataMean)
names(stdDataMean) <- c("subId", "value", "group")

timeData <- filter(groupData, condition == "time")
timeDataMean <- summarise(timeData, mean(value))
timeDataMean$group <- group
timeDataMean <- as.data.frame(timeDataMean)
names(timeDataMean) <- c("subId", "value", "group")



#### wide data ####
dataWide <- cbind(Drt_max.csv$V1, Drt_std.csv$V1, Drt_time.csv$V1,
                Gap_max.csv$V1, Gap_std.csv$V1, Gap_time.csv$V1,
                Frq_max.csv$V1, Frq_std.csv$V1, Frq_time.csv$V1,
                Int_max.csv$V1, Int_std.csv$V1, Int_time.csv$V1,
                Spc_max.csv$V1, Spc_std.csv$V1, Spc_time.csv$V1,
                Std_max.csv$V1, Std_std.csv$V1, Std_time.csv$V1)

dataWide <- as.data.frame(dataWide)
wideNames <- NULL
for (i in seq(temp)){ wideNames[i] <- temp[i] }
wideNames <- str_sub(wideNames, end = -5)
names(dataWide) <- wideNames
group <- factor(c(rep("coma", 5), rep("vs", 5), rep("mcs", 4), rep("control", 15)))
dataWide$subid <- 1:29
dataWide$group <- group
dataWide <- dataWide[-2,]


