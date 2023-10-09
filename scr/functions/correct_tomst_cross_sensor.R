correct_tomst_cross_sensor <- function(tomsts_to_cor, df){
  print(paste0("Calculating correction values for ", length(tomsts_to_cor), " logger(s)..."))
  diffs_all <- tibble()
  for(i in tomsts_to_cor){
    office <- df %>% filter(tomst_id == i) %>% 
      filter(probl == 2) %>% pull(datetime) %>% 
      as_date() %>% unique()
    office <- office[-which(office == max(office))]
    
    df %>% filter(tomst_id == i) %>%
      mutate(date = as_date(datetime)) %>% 
      filter(date %in% office) %>% 
      mutate(change1a = abs(T3 - lag(T3,1)),
             change1b = abs(T3 - lag(T3,2)),
             change1c = abs(T3 - lag(T3,3)),
             change1d = abs(T3 - lag(T3,4)),
             change1e = abs(T3 - lag(T3,5)),
             change1f = abs(T3 - lag(T3,6)),
             change1g = abs(T3 - lead(T3,1)),
             change1h = abs(T3 - lead(T3,2)),
             change1i = abs(T3 - lead(T3,3))) %>% 
      rowwise() %>%
      mutate(change1 = max(change1a, change1b, change1c,
                           change1d, change1e, change1f,
                           change1g, change1g, change1i, na.rm = T)) %>%
      mutate(T3 = ifelse(change1 > 0.1250, NA, T3)) %>% 
      filter(!is.na(T3)) %>% 
      as.data.frame() %>% 
      filter(complete.cases(.)) -> temp
    
    means <- c(T1 = mean(temp$T1),
               T2 = mean(temp$T2),
               T3 = mean(temp$T3))
    
    diffs <- round(means - median(means),4)
    
    print(i)
    print(diffs)
    
    diffs_all <- bind_rows(diffs_all,
                           bind_cols(data.frame(tomst_id = i), 
                                     as.data.frame(t(as.data.frame(diffs)))))
  }
  return(diffs_all)
  
}
