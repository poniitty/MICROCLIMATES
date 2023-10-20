#

# Add zeros if needed until three numbers and combine with area code
add_zeros <- function(x, area){
  xx <- unlist(lapply(x, function(x){
    if(nchar(x) == 1){
      return(as.character(paste0(area,"00",x)))
    }
    if(nchar(x) == 2){
      return(as.character(paste0(area,"0",x)))
    }
    if(nchar(x) > 2){
      return(as.character(paste0(area,x)))
    }
    }))
  return(xx)
}

# Mean function that excludes the mid-most value
# The length of the vector has to be odd number
mean_exclude_middle <-  function(x, na.rm = T) {
  return(mean(x[-ceiling(0.5*length(x))], na.rm = na.rm))
}

# Own helper function to round to two decimals
round2 <- function(x) round(x,2)

# Own function to change infinite values to NA
infmutate <- function(x) ifelse(is.infinite(x),NA,x)

# Quantile with NA handling
own_quantile <- function(x, prob){
  if(any(is.na(x))){
    return(NA)
  } else {
    return(as.numeric(quantile(x,prob)))
  }
}

# Thermal sum calculations

therm_sum <- function(x, thrh, direc, na.rm = F){
  if(direc == "above"){
    return(sum(ifelse(x >= thrh, x, 0), na.rm = na.rm))
  }
  if(direc == "below"){
    return(sum(ifelse(x <= thrh, x, 0), na.rm = na.rm))
  }
}