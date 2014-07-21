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