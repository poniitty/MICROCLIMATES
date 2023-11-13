library(tidyverse)
library(lubridate)
library(cowplot)
library(zoo)

area <- "RAS"

invisible(lapply(list.files("scr/functions/", ".R$", full.names = T), source))

df <- read_csv(paste0("output/",area,"/tomst_data_raw.csv")) %>% 
  mutate(datetime = with_tz(datetime, tzone = "Etc/GMT-2"))


################################################################
# Cross-sensor corrections

ctd <- read_csv("data/Correction_temperatures.csv") %>% 
  rename(T1c = T1,
         T2c = T2,
         T3c = T3)

df <- left_join(df, ctd) %>% 
  mutate(across(c(T1c,T2c,T3c), ~ifelse(is.na(.x), 0, .x))) %>% 
  mutate(T1 = T1 - T1c,
         T2 = T2 - T2c,
         T3 = T3 - T3c) %>% 
  select(-c(T1c,T2c,T3c))

###################################################################
# Remove logger reading from the time of reading the loggers
# as reading the logger may have influenced the measurements

visittimes <- read_csv(paste0("data/",area,"/reading_times_",area,".csv")) %>%
  mutate(maxdt = with_tz(maxdt, tzone = "Etc/GMT-2"))

df <- left_join(df, visittimes %>% mutate(visit = 1) %>% rename(datetime = maxdt)) %>% 
  group_by(site, tomst_id) %>%
  mutate(visit = rollapply(visit, width=3, FUN=sum, na.rm = T, 
                           fill = NA, partial = T, align = "center")) %>% 
  mutate(across(T1:moist, ~ifelse(visit == 0, ., NA))) %>%
  select(-visit)

###################################################################
# Remove unrealistically high and low values and their neighbors

df <- df %>% mutate(T1 = ifelse(rollmax((T1 < (-40) | T1 > 30), 3, align = "center", fill = 0) == 1, NA, T1),
                    T2 = ifelse(rollmax((T2 < (-60) | T2 > 50), 3, align = "center", fill = 0) == 1, NA, T2),
                    T3 = ifelse(rollmax((T3 < (-60) | T3 > 50), 3, align = "center", fill = 0) == 1, NA, T3),
                    moist = ifelse(rollmax((moist < 200 | moist >= 4096), 3, align = "center", fill = 0) == 1, NA, moist))

##############################################################################
# Filter by error code, i.e., remove values reported manually erroneous

# Error code for Tomst TMS-4 logger measurements: 
# 0 = ok
# 1 = Clearly erroneous data (e.g., logger fallen and off the ground)
# 2 = Tomst not in the field.
# 3 = T3 without solar shielding but data may be useful for some purposes, T1 & T2 ok.
# 4 = T2 and moist ok, T1 and T3 problematic
# 5 = T3 erroneous, moist, T1 and T2 ok
# 6 = moist erroneous, temps ok
# 7 = T1 and moist ok, T2 and T3 erroneous
# 8 = Only T1 ok
# 9 = T2 and T3 ok, moist and T1 problematic
# 10 = T1 and T2 ok, moist and T3 problematic

df <- df %>%
  mutate(T1 = as.numeric(ifelse(error_tomst %in% c(1,2,4,9), NA, T1))) %>% 
  mutate(T2 = as.numeric(ifelse(error_tomst %in% c(1,2,7,8), NA, T2))) %>% 
  mutate(T3 = as.numeric(ifelse(error_tomst %in% c(1,2,4,5,7,8,10), NA, T3))) %>% 
  mutate(moist = as.numeric(ifelse(error_tomst %in% c(1,2,6,8,9,10), NA, moist)))
gc()
###################################################################
# Detect and remove erroneous peaks

# Create a log-files to store information about the altered data
sink(paste0("output/script_log_files/",area,"_tomst_postprocess_log_",Sys.Date(),".txt"))

# reference time series

refts <- df %>% 
  group_by(datetime) %>% 
  summarise(across(c(T1,T2,T3,moist), list(mean = ~mean(.x, na.rm = TRUE), sd = ~sd(.x, na.rm = TRUE)), .names = "{col}_{fn}"))

df <- left_join(df,
                refts)
# T1

df <- df %>% 
  mutate(T1_diff = T1 - T1_mean) %>% 
  group_by(site) %>% 
  mutate(T1_diff_roll = rollapply(T1_diff, width = 9, FUN = mean_exclude_middle, fill = NA)) %>% 
  mutate(T1h = T1-lag(T1),
         T1l = T1-lead(T1)) %>% 
  mutate(outlier = FALSE) %>% 
  mutate(outlier = ifelse(abs(T1_diff) > T1_sd*6 & T1_diff > 10, TRUE, outlier),
         # Absolute difference to more 6 times the SD and higher than 10C
         outlier = ifelse(abs(T1_diff) > T1_sd*6 & T1_diff < (-20), TRUE, outlier),
         # Absolute difference to more 6 times the SD and lower than -20C
         outlier = ifelse(abs(T1_diff - T1_diff_roll) > 5, TRUE, outlier),
         # Momentous absolute difference to the smoothed difference to reference timeseries is more than 5C
         outlier = ifelse(abs(T1h) > 5, TRUE, outlier),
         # Absolute difference to previous measurement is higher than 5C
         outlier = ifelse(abs(T1l) > 5, TRUE, outlier),
         # Absolute difference to next measurement is higher than 5C
         outlier = ifelse(T1h > 2 & T1l > 2, TRUE, outlier),
         # Sudden reversible peak is higher than 2C
         outlier = ifelse(T1h < (-2) & T1l < (-2), TRUE, outlier)
         # Sudden reversible drop is lower than 2C
  )

print(paste0("T1 data has ", sum(df$outlier, na.rm = T), " suspicious peaks or drops, that will be deleted"))

df <- df %>% 
  mutate(outlier = ifelse(is.na(outlier), FALSE, outlier)) %>% 
  mutate(T1 = ifelse(rollmax(outlier, 5, align = "center", fill = 0) == 1, NA, T1)) %>% 
  select(site:moist_sd)

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
         outlier = ifelse(T2h > 5 & T2l > 5, TRUE, outlier),
         # Sudden reversible peak is higher than 5C
         outlier = ifelse(T2h < (-5) & T2l < (-5), TRUE, outlier)
         # Sudden reversible drop is lower than 5C
         )

print(paste0("T2 data has ", sum(df$outlier, na.rm = T), " suspicious peaks or drops, that will be deleted"))

df <- df %>% 
  mutate(outlier = ifelse(is.na(outlier), FALSE, outlier)) %>% 
  mutate(T2 = ifelse(rollmax(outlier, 5, align = "center", fill = 0) == 1, NA, T2)) %>% 
  select(site:moist_sd)

# T3

df <- df %>% 
  mutate(T3_diff = T3 - T3_mean) %>% 
  group_by(site) %>% 
  mutate(T3_diff_roll = rollapply(T3_diff, width = 9, FUN = mean_exclude_middle, fill = NA)) %>% 
  mutate(T3h = T3-lag(T3),
         T3l = T3-lead(T3)) %>% 
  mutate(outlier = FALSE) %>% 
  mutate(outlier = ifelse(abs(T3_diff) > T3_sd*8 & T3_diff > 20, TRUE, outlier),
         # Absolute difference to more 8 times the SD and higher than 20C
         outlier = ifelse(abs(T3_diff) > T3_sd*8 & T3_diff < (-20), TRUE, outlier),
         # Absolute difference to more 8 times the SD and lower than -20C
         outlier = ifelse(abs(T3_diff - T3_diff_roll) > 10, TRUE, outlier),
         # Momentous absolute difference to the smoothed difference to reference timeseries is more than 10C
         outlier = ifelse(abs(T3h) > 10, TRUE, outlier),
         # Absolute difference to previous measurement is higher than 10C
         outlier = ifelse(abs(T3l) > 10, TRUE, outlier),
         # Absolute difference to next measurement is higher than 10C
         outlier = ifelse(T3h > 5 & T3l > 5, TRUE, outlier),
         # Sudden reversible peak is higher than 5C
         outlier = ifelse(T3h < (-5) & T3l < (-5), TRUE, outlier)
         # Sudden reversible drop is lower than 5C
  )

print(paste0("T3 data has ", sum(df$outlier, na.rm = T), " suspicious peaks or drops, that will be deleted"))

df <- df %>% 
  mutate(outlier = ifelse(is.na(outlier), FALSE, outlier)) %>% 
  mutate(T3 = ifelse(rollmax(outlier, 5, align = "center", fill = 0) == 1, NA, T3)) %>% 
  select(site:moist_sd)

# moist

df <- df %>% 
  mutate(moist_diff = moist - moist_mean) %>% 
  group_by(site) %>% 
  mutate(moist_diff_roll = rollapply(moist_diff, width = 9, FUN = mean_exclude_middle, fill = NA)) %>% 
  mutate(moisth = moist-lag(moist),
         moistl = moist-lead(moist)) %>% 
  mutate(outlier = FALSE) %>% 
  mutate(outlier = ifelse(abs(moist_diff - moist_diff_roll) > 1000, TRUE, outlier),
         # Momentous absolute difference to the smoothed difference to reference timeseries is more than 1000
         outlier = ifelse(abs(moisth) > 1500, TRUE, outlier),
         # Absolute difference to previous measurement is higher than 1500
         outlier = ifelse(abs(moistl) > 1000, TRUE, outlier),
         # Absolute difference to next measurement is higher than 1000
         outlier = ifelse(moisth > 300 & moistl > 300, TRUE, outlier),
         # Sudden reversible peak is higher than 300
         outlier = ifelse(moisth < (-100) & moistl < (-100), TRUE, outlier)
         # Sudden reversible drop is lower than 100
  )

print(paste0("moist data has ", sum(df$outlier, na.rm = T), " suspicious peaks or drops, that will be deleted"))

df <- df %>% 
  mutate(outlier = ifelse(is.na(outlier), FALSE, outlier)) %>% 
  mutate(moist = ifelse(rollmax(outlier, 5, align = "center", fill = 0) == 1, NA, moist)) %>% 
  select(site:error_tomst) %>% 
  ungroup()
sink()

####################################################################################
# Fill NA rows if missing time-stamps

tids <- unique(df$site)

df <- lapply(tids, fill_timestamps_site, df = df) %>% 
  bind_rows()

df %>% write_csv(paste0("output/",area,"/tomst_data_cleaned.csv"))
