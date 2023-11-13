
# i <- f[11]
# read_tomst_data(f[11])
# data_dir <- raw_data_dir
read_tomst_data <- function(i, data_dir, tzone = "Etc/GMT-2"){
  print(paste0("Reading... ",i))
  d <- fread(i)
  if(nrow(d) > 100){
    if(!"V1" %in% names(d)){
      stop("Column names something, I'm not used to. Consider editing the reading function")
    }
    d %>% select(V2,V3,V4,V5,V6,V7) -> d
    
    d <- d %>% 
      mutate(V2 = gsub("/","\\.",V2)) %>% 
      mutate(V2 = ifelse(nchar(V2) == 10, paste0(V2, " 00:00:00"), V2))
    
    d %>% filter(!duplicated(.$V2, fromLast = T)) -> d
    
    d %>% mutate(across(V4:V6, ~as.numeric(gsub(",",".\\",.)))) -> d
    
    d$tomst_id <- as.numeric(strsplit(gsub("data_","",rev(strsplit(i, "/")[[1]])[1]), "_")[[1]][1])
    
    ft <- d$V2[1]
    
    if(nchar(ft) == 19){
      if(nchar(str_split(ft, "\\.")[[1]][1]) < 4){
        d <- d %>% mutate(V2 = dmy_hms(V2, tz = "UTC"))
      } else {
        d <- d %>% mutate(V2 = ymd_hms(V2, tz = "UTC"))
      }
    } else {
      if(nchar(str_split(ft, "\\.")[[1]][1]) < 4){
        d <- d %>% mutate(V2 = dmy_hm(V2, tz = "UTC"))
      } else {
        d <- d %>% mutate(V2 = ymd_hm(V2, tz = "UTC"))
      }
    }
    
    d %>% mutate(V2 = with_tz(V2, tzone = tzone)) %>% 
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