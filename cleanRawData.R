#this cleans the raw data and then writes out a 55845 row table.
#uses SydneyCleanData as in input.
rm(list = ls())
fnCumDays <- function(intMonth, intDay) {
    if (intMonth == 1) intCumMonthDays = 0                  #January
    else if (intMonth == 2) intCumMonthDays = 31            #February
    else if (intMonth == 3) intCumMonthDays = 59            #March
    else if (intMonth == 4) intCumMonthDays = 90            #April
    else if (intMonth == 5) intCumMonthDays = 120           #May
    else if (intMonth == 6) intCumMonthDays = 151           #June
    else if (intMonth == 7) intCumMonthDays = 181           #July
    else if (intMonth == 8) intCumMonthDays = 212           #August
    else if (intMonth == 9) intCumMonthDays = 243           #September
    else if (intMonth == 10) intCumMonthDays = 273          #October
    else if (intMonth == 11) intCumMonthDays = 304          #November
    else if (intMonth == 12) intCumMonthDays = 334          #December
    else intCumMonthDays = NA                               #ERROR  
    return(intCumMonthDays + intDay)
}

setwd('/Users/zurich/Google Drive/CURRENT-TO-BE-MOVED/ContourMap')
origData <- read.csv('SydneyCleanData.csv', header = FALSE)
names(origData) <- c('day','month','year','date','temp')

#get rid of partial year
origData <- subset(origData, !(year==2012)) 
#get rid of leap days
origData <- subset(origData, !(month==2 & day ==29)) 
#daynumber from 1..365 for each year
origData$cumDay <- mapply(fnCumDays, origData$month, origData$day)
#years since 1858
origData$yearNumber <- as.factor(sapply(origData$year, function(x) x-1858)) 

write.table(origData, "cleanRaw.csv", 
            row.names=FALSE, col.names=TRUE, sep=",")

