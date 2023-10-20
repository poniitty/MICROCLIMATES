# Calibration function for the moisture count values for unknown soils from Kopecky et al. 2020
cal_funNA <- function(x) {((-1.34e-8) * (x^2) + (2.50e-4) * x + (-1.58e-1))*100 }
