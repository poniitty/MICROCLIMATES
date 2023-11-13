library(tidyverse)
library(missRanger, lib.loc = "/projappl/project_2003061/Rpackages")
library(lubridate)
library(data.table)
library(zoo)

area <- "PIS"

invisible(lapply(list.files("scr/functions/", ".R$", full.names = T), source))

df <- fread(paste0("output/",area,"/tomst_data_raw.csv")) %>% 
  mutate(datetime = with_tz(datetime, tzone = "Etc/GMT-2")) %>% 
  select(site, tomst_id, datetime, T2, error_tomst)

################################################################
# Cross-sensor corrections

ctd <- read_csv("data/Correction_temperatures.csv") %>% 
  select(tomst_id, T2) %>% 
  rename(T2c = T2)

df <- left_join(df, ctd) %>% 
  mutate(across(c(T2c), ~ifelse(is.na(.x), 0, .x))) %>% 
  mutate(T2 = T2 - T2c) %>% 
  select(-c(T2c))

###################################################################
# Remove logger reading from the time of reading the loggers
# as reading the logger may have influenced the measurements

visittimes <- read_csv(paste0("data/",area,"/reading_times_",area,".csv")) %>%
  mutate(maxdt = with_tz(maxdt, tzone = "Etc/GMT-2"))

df <- left_join(df, visittimes %>% mutate(visit = 1) %>% rename(datetime = maxdt)) %>% 
  group_by(site, tomst_id) %>%
  mutate(visit = rollapply(visit, width=3, FUN=sum, na.rm = T, 
                           fill = NA, partial = T, align = "center")) %>% 
  mutate(across(T2, ~ifelse(visit == 0, ., NA))) %>%
  select(-visit)

###################################################################
# Remove unrealistically high and low values and their neighbors

df <- df %>% mutate(T2 = ifelse(rollmax((T2 < (-60) | T2 > 50), 3, align = "center", fill = 0) == 1, NA, T2)) %>% 
  ungroup()

##############################################################################
# Filter by error code, i.e., remove values reported manually erroneous

df <- df %>%
  mutate(T2 = as.numeric(ifelse(error_tomst %in% c(2), NA, T2)))
gc()
###################################################################
# Detect and remove erroneous peaks

# Check measurement intervals

sites <- unique(df$site)

frqs <- lapply(sites, function(i){
  df %>% filter(site == i) -> temp
  temp %>% mutate(timediff = as.numeric(datetime - lag(datetime))) -> temp
  x <- unique(na.omit(temp$timediff))
  names(x) <- i
  return(x)
})
names(frqs) <- sites
table(unlist(frqs)) # Many sites have various measurement intervals
frqs[which(unlist(frqs) == 10)] # None

# Force this one site to 15 min intervals

if(any(unlist(lapply(frqs, length)) > 1) | any(unlist(frqs) == 10)){
  
  df <- lapply(frqs, function(i){
    
    if((length(i) > 1 )| any(i == 10)){
      print(names(i))
      
      temp <- df %>% filter(site == names(i)[1]) %>% 
        filter(!is.na(datetime))
      
      temp <- full_join(temp,
                        tibble(datetime = seq(min(temp$datetime), max(temp$datetime), by = '15 mins'),
                               keep = TRUE)) %>%
        arrange(datetime) %>%
        mutate(across(T2, ~na.approx(.x, datetime, maxgap = 1, na.rm = F))) %>%
        filter(keep) %>%
        fill(site, tomst_id, error_tomst) %>%
        select(-keep)
      return(temp)
    } else {
      return(df %>% filter(site == names(i)))
    }
  }) %>% 
    bind_rows()
  gc()
  
  frqs <- lapply(sites, function(i){
    df %>% filter(site == i) -> temp
    temp %>% mutate(timediff = as.numeric(datetime - lag(datetime))) -> temp
    x <- unique(na.omit(temp$timediff))
    names(x) <- i
    return(x)
  })
  names(frqs) <- sites
  table(unlist(frqs)) # All have now 15 min intervals
  frqs[which(unlist(frqs) == 10)] # None, Good
}

#######################################################################
# reference time series for peak identification
gc()
refts <- df %>% 
  group_by(datetime) %>% 
  summarise(across(c(T2), list(mean = ~mean(.x, na.rm = TRUE), sd = ~sd(.x, na.rm = TRUE)), .names = "{col}_{fn}"))

df <- left_join(df,
                refts)
gc()


# T2

df <- df %>% 
  mutate(T2_diff = T2 - T2_mean) %>% 
  group_by(site) %>% 
  mutate(T2_diff_roll = rollapply(T2_diff, width = 9, FUN = mean_exclude_middle, fill = NA)) %>% 
  mutate(T2h = T2-lag(T2),
         T2l = T2-lead(T2)) %>% 
  mutate(outlier = FALSE) %>% 
  mutate(outlier = ifelse(abs(T2_diff) > T2_sd*8 & T2_diff > 20, TRUE, outlier),
         # Absolute difference to more 8 times the SD and higher than 20C
         outlier = ifelse(abs(T2_diff) > T2_sd*8 & T2_diff < (-20), TRUE, outlier),
         # Absolute difference to more 8 times the SD and lower than -20C
         outlier = ifelse(abs(T2_diff - T2_diff_roll) > 10, TRUE, outlier),
         # Momentous absolute difference to the smoothed difference to reference timeseries is more than 10C
         outlier = ifelse(abs(T2h) > 10, TRUE, outlier),
         # Absolute difference to previous measurement is higher than 10C
         outlier = ifelse(abs(T2l) > 10, TRUE, outlier),
         # Absolute difference to next measurement is higher than 10C
         outlier = ifelse(T2h > 5 & T2l > 5, TRUE & abs(T2_diff - T2_diff_roll) > 5, outlier),
         # Sudden reversible peak is higher than 5C
         outlier = ifelse(T2h < (-5) & T2l < (-5) & abs(T2_diff - T2_diff_roll) > 5, TRUE, outlier)
         # Sudden reversible drop is lower than 5C
  )

print(paste0("T2 data has ", sum(df$outlier, na.rm = T), " suspicious peaks or drops, that will be deleted"))

df <- df %>% 
  mutate(outlier = ifelse(is.na(outlier), FALSE, outlier)) %>% 
  mutate(T2 = ifelse(rollmax(outlier, 5, align = "center", fill = 0) == 1, NA, T2)) %>% 
  select(site:T2) %>% 
  ungroup()

# T2

dfw <- df %>% 
  select(site, datetime, T2) %>% 
  pivot_wider(id_cols = datetime, names_from = site, values_from = T2) %>% 
  arrange(datetime) %>% 
  mutate(id = 1:nrow(.),
         yd = yday(datetime),
         hour = hour(datetime)) %>% 
  relocate(id, yd, hour) %>% 
  select(-datetime)

# Imputation, this may take tens of minutes...
dfwimp <- missRanger(dfw,
                     pmm.k = 3, num.trees = 100, maxiter = 10,
                     num.threads = future::availableCores())


df <- df %>% 
  select(site, datetime, T2) %>% 
  pivot_wider(id_cols = datetime, names_from = site, values_from = T2) %>% 
  arrange(datetime) %>% 
  mutate(id = 1:nrow(.)) %>% 
  select(datetime, id) %>% 
  left_join(.,
            dfw %>% 
              select(-yd,-hour) %>% 
              pivot_longer(cols = c(-id), names_to = "site", values_to = "T2") %>% 
              mutate(T2_imp = ifelse(is.na(T2), TRUE, FALSE)) %>% 
              select(-T2)) %>% 
  left_join(.,
            dfwimp %>% 
              select(-yd,-hour) %>% 
              pivot_longer(cols = c(-id), names_to = "site", values_to = "T2")) %>% 
  arrange(site, datetime) %>% 
  select(-id)

##############################################################
# Snow

snws <- snow_derivs(df_micro = df)

write_csv(snws, paste0("output/",area,"/tomst_snow_variables.csv"))
