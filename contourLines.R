rm(list = ls())
setwd('/Users/zurich/Google Drive/CURRENT-TO-BE-MOVED/ContourMap')
library(classInt); library(ggplot2); library(sp); library(fields)
library(maptools)
#load function
source("fnQuantile.r")



tData <- read.csv("cleanRaw.csv", header = TRUE, sep = ",")
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

#now run the Thin Plate Spline
tps <- Tps(coordinates(spDf), spDf$temp)
SPDF$spl_pred <- predict(tps, coordinates(SPDF))

#filled contours
intDivNum <- 10
cr <- colorRampPalette(c("#b9e2ef", "#1d62a0"))
cGrad <- cr(intDivNum)
ci <- classIntervals(tData$temp, n = intDivNum, style = "quantile")
#intNumXCells * intNumYCells
image(SPDF, "spl_pred", breaks = ci$brks, col = cGrad)

#contour lines

intDivNum2 <- 20
colVect <- c("#a50026","#d73027","#f46d43","#fdae61","#fee090",
             "#e0f3f8","#abd9e9","#74add1","#4575b4","#313695")
colVect <- rev(colVect)
cr2 <- colorRampPalette(colVect)
cGrad2 <- cr2(intDivNum2-4)
ci2 <- classIntervals(tData$temp, n = intDivNum2, style = "quantile")

im <- as.image.SpatialGridDataFrame(SPDF["spl_pred"])
clReturn <- contourLines(im, nlevels = intDivNum2, levels = ci2$brks)
#ContourLines2SLDF converts contourLines into a SpatialLinesDF
cl <- ContourLines2SLDF(clReturn)
#sp.layout=list("sp.lines", fill=0, lwd=3)
#spplot(cl, col.regions = cGrad2, sp.layout=list("sp.lines", cl, col = cGrad2, lwd=3))
spplot(cl, col.regions = cGrad2)


