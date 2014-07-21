rm(list = ls())

library(classInt);library(ggplot2); library(RCurl)
fnQuantile <- function(vctInput, intNumberDivisions) {
    #returns a factor. Slices up vctInput into intNumberDivisions. 
    #Each slice contains approx same number of observations. Return vector is
    #the same length as input
    vctDivisions <- seq(0,1, by = 1 / intNumberDivisions)
    vctQuantile <- quantile(vctInput, probs = vctDivisions)
    vctFactor <- cut(vctInput, vctQuantile, 
                     labels = 1:intNumberDivisions, include.lowest=TRUE)    
    return(vctFactor)
}

fnReturnColorNumber <- function(obsValue)  {
    colorNumber <- findInterval(obsValue, ci$brks)
    return(colorNumber)
}


x <- getURL("https://raw.githubusercontent.com/thefactmachine/thin-plate-splines/master/cleanRaw.csv")
tData <- read.csv(text = x,  header = TRUE, sep = ",")





# 1) Aggregation stage. Original data is 365 x 153
# 86 x 153 is 1.77 aspect
tData$dayBin <-fnQuantile(tData$cumDay, 20)
tData$yearBin<- fnQuantile(tData$yearNumber, 35)
dfAgg <- aggregate(temp ~ dayBin + yearBin, data=tData, mean)


intDivNum <- 40
ci <- classIntervals(tData$temp, n = intDivNum, style = "quantile")
dfAgg$newFact <- sapply(dfAgg$temp, fnReturnColorNumber)

# 3) make the plot

dfAgg$newFact  <- (dfAgg$newFact / 4) + 2.0

p <- ggplot(dfAgg , aes(x= yearBin, y = dayBin))
p <- p + geom_point(size = dfAgg$newFact, colour = "#000000")
p <- p + coord_equal()
p <- p + theme(axis.line=element_blank(),
               axis.text.x=element_blank(),
               axis.text.y=element_blank(),
               axis.ticks=element_blank(),
               axis.title.x=element_blank(),
               axis.title.y=element_blank(),
               legend.position="none",
               panel.background=element_blank(),
               panel.border=element_blank(),
               panel.grid.major=element_blank(),
               panel.grid.minor=element_blank(),
               plot.background=element_blank())
p <- p + theme(panel.background = element_rect(fill = "white"))
p


