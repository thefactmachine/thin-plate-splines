rm(list = ls())

library(classInt); library(ggplot2); library(sp); library(fields); library(RCurl)
#load function
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


x <- getURL("https://raw.githubusercontent.com/thefactmachine/thin-plate-splines/master/cleanRaw.csv")
tData <- read.csv(text = x,  header = TRUE, sep = ",")




#need to create aggregates (i.e.bins) Starting with 365 x 153
#want a visually pleasing 1.7 (16:9) aspect
tData$dayBin <-as.numeric(fnQuantile(tData$cumDay, 20))
tData$yearBin<- as.numeric(fnQuantile(tData$yearNumber, 35))
dfAgg <- aggregate(temp ~ dayBin + yearBin, data=tData, mean)


#create SpatialPoints from yearNum, dayNum
sp <- SpatialPoints(as.matrix(dfAgg[, c(2,1)]))
#spDF needs a dataframe
dftemp <- data.frame(temp = dfAgg$temp)
spDf <-  SpatialPointsDataFrame(sp , dftemp)

#long process of creating SpatialPixelsDataFrame
bb <- bbox(spDf)
XRange <- bb[1,2] - bb[1,1]; YRange <- bb[2,2] - bb[2,1]
intMinX <- bb[1,1]; intMinY <- bb[2,1]; intMaxX <- bb[1,2]; intMaxY <- bb[2,2]

#cellSize of 0.01 results in 6 meg image
cellSize <- 0.1
intNumXCells <- (XRange %/% cellSize) + 1
intNumYCells <- (YRange %/% cellSize) + 1
grille <- GridTopology(c(intMinX, intMinY), 
            c(cellSize, cellSize), c(intNumXCells, intNumYCells))

#this can be used to cut a hold in the grid.  But we don't need that here
inside0 <- rep(0, intNumXCells*intNumYCells)
SGDF <- SpatialGridDataFrame(grille, data = data.frame(list(ins = inside0)))
SPDF <- as(SGDF, "SpatialPixelsDataFrame")

#Run the This Plate Spline.
#1) Construct. the model. 
#coordinates() returns x, y values, spDf$temp is the Z
tps <- Tps(coordinates(spDf), spDf$temp)

#2) SPDF has 6.5 million cells.
SPDF$spl_pred <- predict(tps, coordinates(SPDF))

#Number of contour bands
intDivNum <- 20

#set of 10 colors from colorbrewer2.org
colVect <- c("#a50026","#d73027","#f46d43","#fdae61","#fee090",
             "#e0f3f8","#abd9e9","#74add1","#4575b4","#313695")

#turn 10 colors into 20, first get function from colorRampPalette()
cr <- colorRampPalette(rev(colVect))

#next apply the fuction. cGrad contains 20 colours
cGrad <- cr(intDivNum)

#slice the temperatures into 20 bins
ci <- classIntervals(tData$temp, n = intDivNum, style = "quantile")
image(SPDF, "spl_pred", breaks = ci$brks, col = cGrad)

 


