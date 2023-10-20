
# i <- f[1]
# data_dir <- raw_data_dir
read_tomst_data <- function(i, data_dir, tzone = "Etc/GMT-2"){
  print(paste0("Reading... ",i))
  d <- fread(i)
  if(nrow(d) > 100){
    if(!"V1" %in% names(d)){
      stop("Column names something, I'm not used to. COnsider editing the reading function")
    }
    d %>% select(V2,V3,V4,V5,V6,V7) -> d
    
    d %>% filter(!duplicated(.$V2, fromLast = T)) -> d
    
    d %>% mutate(across(V4:V6, ~as.numeric(gsub(",",".\\",.)))) -> d
    
    d$tomst_id <- as.numeric(strsplit(gsub("data_","",rev(strsplit(i, "/")[[1]])[1]), "_")[[1]][1])
    
    d %>% mutate(V2 = ymd_hm(V2, tz = "UTC")) %>% 
      mutate(V2 = with_tz(V2, tzone = tzone)) %>% 
      rename(datetime = V2,
             zone = V3,
             T1 = V4,
             T2 = V5,
             T3 = V6,
             moist = V7) %>% 
      relocate(tomst_id) -> d
    
    return(d)
  } else {
    return(NULL)
  }
  
}