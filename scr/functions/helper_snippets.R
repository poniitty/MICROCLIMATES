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
