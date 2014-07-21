thin-plate-splines
==================

Uses thin plate splines to interpolated between two data dimensions and create contour plots

contour.R				    uses cleanRaw.csv and makes contour blue lines. 
contourFIll.R				uses cleanRaw.csv makes colored contour
contourLines.R			uses cleanRaw.csv and makes contour coloured lines.
heatMap-colour.R		uses cleanRaw.csv and creates graduated blue heat map
heatMap-size.R			uses cleanRaw.csv and creates black and white heat map using circles with varying circle size. 

cleanRawData.R			uses SydneyCleanData.csv writes out clean raw.  Takes out leap years calculates cum days


fnQuantile.R				used by everything. creates quantiles.  Embedded in each R file to prevent calls to Git

