library(tidyverse)
library(data.table)
library(scales)
library(zoo)
library(broom)

area <- "RAS"

invisible(lapply(list.files("scr/functions/", ".R$", full.names = T), source))

df <- fread(paste0("output/",area,"/tomst_data_imputed.csv")) %>% 
  mutate(datetime = with_tz(datetime, tzone = "Etc/GMT-2")) %>% 
  filter(!is.na(datetime)) %>% 
  filter(!is.na(site))

# Calibrate the moisture values
df <- df %>% 
  mutate(moist = round(cal_funNA(moist),1))


####################################################################
# Thermal stuff

dtvs <- microclim_derivs(df_micro = df, quant_max = 0.999, quant_min = 0.001)

write_csv(dtvs, paste0("output/",area,"/tomst_temperature_variables.csv"))

##########################################################
# Moisture

mtvs <- moisture_derivs(df_micro = df)

write_csv(mtvs, paste0("output/",area,"/tomst_moisture_variables.csv"))

