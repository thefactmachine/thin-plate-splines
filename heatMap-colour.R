rm(list = ls())
setwd('/Users/zurich/Google Drive/CURRENT-TO-BE-MOVED/ContourMap')
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
library(classInt)
library(ggplot2)
intDivNum <- 11

tData <- read.csv("cleanRaw.csv", header = TRUE, sep = ",")
#need to create aggregates. (i.e. bins) Starting with 365 x 153
# 86 x 153 is 1.77 aspect
tData$dayBin <-fnQuantile(tData$cumDay, 84)
tData$yearBin<- fnQuantile(tData$yearNumber, 153)
dfAgg <- aggregate(temp ~ dayBin + yearBin, data=tData, mean)

fnReturnColorNumber <- function(obsValue)  {
    colorNumber <- findInterval(obsValue, ci$brks)
    return(colorNumber)
}


vctCol <- c("#d53e4f","#fc8d59","#fee08b","#ffffbf","#e6f598","#99d594","#3288bd")
# rev = revese
cr2 <- colorRampPalette(rev(vctCol), space = "Lab", bias = 0.8)
cGrad <- cr2(intDivNum +1)
ci <- classIntervals(tData$temp, n = intDivNum, style = "quantile")

dfAgg$newFact <- sapply(dfAgg$temp, fnReturnColorNumber)

library(ggplot2)
p <- ggplot(dfAgg , aes(x= yearBin, y = dayBin))
p <- p + geom_tile(aes(fill = newFact), color = "white")
p <- p + scale_fill_gradient(low = "#e2e6e8", high = "#1d62a0")
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
p <- p + theme(panel.background = element_rect(fill = "black"))
p
