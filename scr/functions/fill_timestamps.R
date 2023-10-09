fill_timestamps <- function(tomstid, df){
  #tomstid <- "94194336"
  
  df %>% filter(tomst_id == tomstid) -> temp
  
  temp %>% mutate(timediff = as.numeric(datetime - lag(datetime))) -> temp
  temp[1,"timediff"] <- 15
  holes <- table(temp$timediff)
  
  if(max(temp$timediff, na.rm = T) > 15){
    
    print(tomstid)
    
    missingt <- c()
    for(ii in which(temp %>% pull(timediff) > 15)){
      
      temp %>% slice((ii-1):(ii+1)) %>% pull(timediff) -> diffs
      
      if(diffs[1] %% 15 == 0L){
        seq(temp %>% slice(ii-1) %>% pull(datetime),
            temp %>% slice(ii) %>% pull(datetime), by = "15 mins") -> seqs
        
        missingt <- c(missingt, 
                      as.character(seqs[which(!seqs %in% (temp %>% slice((ii-1):(ii+1)) %>% pull(datetime)))]))
        
      } else {
        seq(temp %>% slice(ii-1) %>% pull(datetime),
            temp %>% slice(ii) %>% pull(datetime), by = "10 mins") -> seqs
        
        missingt <- c(missingt, 
                      as.character(seqs[which(!seqs %in% (temp %>% slice((ii-1):(ii+1)) %>% pull(datetime)))]))
        
      }
    }
    
    missingdf <- data.frame(datetime = ymd_hms(missingt, tz = lubridate::tz(temp$datetime)),
                            tomst_id = as.numeric(tomstid))
    
    print(paste0("Adding ", nrow(missingdf), " empty row(s)"))
    
    temp %>% full_join(., missingdf, by = c("datetime", "tomst_id")) %>% 
      arrange(site, datetime) %>% 
      select(-timediff) %>% 
      fill(site, probl) -> temp
    
  } else {
    
    temp %>% select(-timediff) -> temp
    
  }
  
  return(temp)
}

# Same but using site instead of tomst id
fill_timestamps_site <- function(siteid, df){
  #siteid <- "VAR001"
  
  df %>% filter(site == siteid) -> temp
  
  temp %>% mutate(timediff = as.numeric(datetime - lag(datetime))) -> temp
  temp[1,"timediff"] <- 15
  holes <- table(temp$timediff)
  
  if(max(temp$timediff, na.rm = T) > 15){
    
    print(siteid)
    
    missingt <- c()
    for(ii in which(temp %>% pull(timediff) > 15)){
      
      temp %>% slice((ii-1):(ii+1)) %>% pull(timediff) -> diffs
      
      if(diffs[1] %% 15 == 0L){
        seq(temp %>% slice(ii-1) %>% pull(datetime),
            temp %>% slice(ii) %>% pull(datetime), by = "15 mins") -> seqs
        
        missingt <- c(missingt, 
                      as.character(seqs[which(!seqs %in% (temp %>% slice((ii-1):(ii+1)) %>% pull(datetime)))]))
        
      } else {
        seq(temp %>% slice(ii-1) %>% pull(datetime),
            temp %>% slice(ii) %>% pull(datetime), by = "10 mins") -> seqs
        
        missingt <- c(missingt, 
                      as.character(seqs[which(!seqs %in% (temp %>% slice((ii-1):(ii+1)) %>% pull(datetime)))]))
        
      }
    }
    
    missingdf <- data.frame(datetime = ymd_hms(missingt, tz = lubridate::tz(temp$datetime)),
                            site = siteid)
    
    print(paste0("Adding ", nrow(missingdf), " empty row(s)"))
    
    temp %>% full_join(., missingdf, by = c("datetime", "site")) %>% 
      arrange(site, datetime) %>% 
      select(-timediff) %>% 
      fill(site, error_tomst) -> temp
    
  } else {
    
    temp %>% select(-timediff) -> temp
    
  }
  
  return(temp)
}
