fill_timestamps <- function(tomstid, df){
  #tomstid <- "94212204"
  
  df %>% ungroup %>% filter(tomst_id == tomstid) -> temp
  
  temp %>% arrange(datetime) %>% mutate(timediff = as.numeric(datetime - lag(datetime))) -> temp
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
    
    missingdf <- data.frame(datetime = as_datetime(missingt),
                            tomst_id = as.numeric(tomstid)) %>% 
      mutate(datetime = with_tz(datetime, tzone = lubridate::tz(temp$datetime))) %>% 
      tibble
    
    print(paste0("Adding ", nrow(missingdf), " empty row(s)"))
    
    temp %>% full_join(., missingdf, by = c("datetime", "tomst_id")) %>% 
      fill(site) %>% 
      arrange(site, datetime) %>% 
      select(-timediff) %>% 
      fill(probl) -> temp
    
  } else {
    
    temp %>% select(-timediff) -> temp
    
  }
  
  return(temp)
}

# Same but using site instead of tomst id
fill_timestamps_site <- function(siteid, df){
  #siteid <- "RAS001"
  
  df %>% ungroup %>% filter(site == siteid) -> temp
  
  temp %>% arrange(datetime) %>% mutate(timediff = as.numeric(datetime - lag(datetime))) -> temp
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
                      as.numeric(seqs[which(!seqs %in% (temp %>% slice((ii-1):(ii+1)) %>% pull(datetime)))]))
        
      } else {
        seq(temp %>% slice(ii-1) %>% pull(datetime),
            temp %>% slice(ii) %>% pull(datetime), by = "10 mins") -> seqs
        
        missingt <- c(missingt, 
                      as.character(seqs[which(!seqs %in% (temp %>% slice((ii-1):(ii+1)) %>% pull(datetime)))]))
        
      }
    }
    
    missingdf <- data.frame(datetime = as_datetime(missingt),
                            site = siteid) %>% 
      mutate(datetime = with_tz(datetime, tzone = lubridate::tz(temp$datetime))) %>% 
      tibble
    
    print(paste0("Adding ", nrow(missingdf), " empty row(s)"))
    
    temp %>% full_join(., missingdf, by = c("datetime", "site")) %>% 
      fill(site) %>% 
      arrange(site, datetime) %>% 
      select(-timediff) %>% 
      fill(tomst_id, error_tomst) -> temp
    
  } else {
    
    temp %>% select(-timediff) -> temp
    
  }
  
  return(temp)
}
