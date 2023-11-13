library(tidyverse)
library(lubridate)
library(data.table)
library(cowplot)
library(zoo)

area <- "EVO"

invisible(lapply(list.files("scr/functions/", ".R$", full.names = T), source))

# Set date limits to remove implausible dates
mind <- as.Date("2018-06-01", tz = "Etc/GMT-2")
maxd <- as.Date("2023-11-01", tz = "Etc/GMT-2")

raw_data_dir <- "/scratch/project_2007415/DATA2023/EVO2023"
# old_data_dir <- "/projappl/project_2003061/repos/microclim_evo/output/"
temp_dir <- "/scratch/project_2007415/temp/"

#################################################################################3
# Read earlier visiting/reading times

visittimes <- read_csv(paste0("data/",area,"/reading_times_",area,".csv")) %>%
  mutate(maxdt = with_tz(maxdt, tzone = "Etc/GMT-2"))

# List logger data files to read
f <- list.files(raw_data_dir, pattern = "data_", full.names = T, recursive = T)

df <- lapply(f, read_tomst_data) %>% 
  rbindlist()

df <- df %>% 
  distinct(tomst_id, datetime, .keep_all = T) %>% 
  arrange(tomst_id, datetime)

df <- left_join(df,
                visittimes %>% group_by(tomst_id) %>% 
                  arrange(tomst_id, desc(maxdt)) %>% 
                  slice(1) %>% select(-maxdt),
                by = "tomst_id") %>% 
  relocate(site)

# Check if all got site names
df %>% filter(is.na(site))
# This should be none

# Calculate new latest reading times
df %>% group_by(site, tomst_id) %>% 
  summarise(maxdt = max(datetime)) -> maxdt

# Check if these dates make sense
maxdt %>% arrange(maxdt)
maxdt %>% arrange(desc(maxdt))

# Combine with previous years visiting times
visittimes <- bind_rows(visittimes,
                        maxdt) %>% 
  distinct()

# Write out
write_csv(visittimes, paste0("data/",area,"/reading_times_",area,".csv"))

# Remove implausible dates
df %>% filter(datetime > mind,
              datetime < maxd) -> df
# df %>% filter(duplicated(df %>% select(site, datetime)))

sites <- unique(df$site)

###################################################################################################################
# Plot timeseries month by month

# Months to plot
times <- seq(floor_date(as_date(min(df$datetime)), "month"),
             ceiling_date(as_date(max(df$datetime)), "month") + months(1) - days(1),
             by = "month")

# Plot each site month by month
monthly_tomst_plots(siteids = sites, df = df, visdir = temp_dir, months_to_plot = times)

#################################################################################
# Screening each site for possible errors
#################################################################################
# Screening each site for possible errors

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

# List new error dates and codes, see the codes above
nel <- bind_rows(
  tibble(site = "EVO001", tomst_id = 94214145, start_date = as_date("2021−05−01"), end_date = as_date("2021−10−25"), probl = 2),
  tibble(site = "EVO007", tomst_id = 94214121, start_date = as_date("2021−05−01"), end_date = as_date("2021−10−19"), probl = 2),
  tibble(site = "EVO007", tomst_id = 94214121, start_date = as_date("2021−10−20"), end_date = as_date("2021−10−21"), probl = 1),
  tibble(site = "EVO007", tomst_id = 94214121, start_date = as_date("2021−10−22"), end_date = as_date("2021−10−25"), probl = 2),
  tibble(site = "EVO010", tomst_id = 94214148, start_date = as_date("2021−05−01"), end_date = as_date("2021−10−24"), probl = 2),
  tibble(site = "EVO013", tomst_id = 94214102, start_date = as_date("2021−05−01"), end_date = as_date("2021−10−19"), probl = 2),
  tibble(site = "EVO013", tomst_id = 94214102, start_date = as_date("2021−10−20"), end_date = as_date("2021−10−25"), probl = 1),
  tibble(site = "EVO013", tomst_id = 94214102, start_date = as_date("2021−10−26"), end_date = as_date("2022−11−22"), probl = 8),
  tibble(site = "EVO014", tomst_id = 94214310, start_date = as_date("2021−05−01"), end_date = as_date("2021−10−19"), probl = 2),
  tibble(site = "EVO014", tomst_id = 94214310, start_date = as_date("2021−10−20"), end_date = as_date("2021−10−25"), probl = 1),
  tibble(site = "EVO014", tomst_id = 94214310, start_date = as_date("2021−10−26"), end_date = as_date("2022−11−22"), probl = 8),
  tibble(site = "EVO016", tomst_id = 94214116, start_date = as_date("2021−05−01"), end_date = as_date("2021−10−19"), probl = 2),
  tibble(site = "EVO016", tomst_id = 94214116, start_date = as_date("2021−10−20"), end_date = as_date("2021−10−21"), probl = 1),
  tibble(site = "EVO016", tomst_id = 94214116, start_date = as_date("2021−10−22"), end_date = as_date("2021−10−25"), probl = 2),
  tibble(site = "EVO017", tomst_id = 94214104, start_date = as_date("2021−05−01"), end_date = as_date("2021−10−19"), probl = 2),
  tibble(site = "EVO017", tomst_id = 94214104, start_date = as_date("2021−10−20"), end_date = as_date("2021−10−24"), probl = 1),
  tibble(site = "EVO017", tomst_id = 94214104, start_date = as_date("2021−10−25"), end_date = as_date("2022−11−22"), probl = 8),
  tibble(site = "EVO018", tomst_id = 94214123, start_date = as_date("2021−05−01"), end_date = as_date("2021−10−19"), probl = 2),
  tibble(site = "EVO018", tomst_id = 94214123, start_date = as_date("2021−10−20"), end_date = as_date("2021−10−21"), probl = 1),
  tibble(site = "EVO018", tomst_id = 94214123, start_date = as_date("2021−10−22"), end_date = as_date("2021−10−25"), probl = 2),
  tibble(site = "EVO020", tomst_id = 94214135, start_date = as_date("2021−05−01"), end_date = as_date("2021−10−25"), probl = 2),
  tibble(site = "EVO025", tomst_id = 94214144, start_date = as_date("2021−05−01"), end_date = as_date("2021−10−24"), probl = 2),
  tibble(site = "EVO028", tomst_id = 94214133, start_date = as_date("2021−05−01"), end_date = as_date("2021−10−25"), probl = 2),
  tibble(site = "EVO029", tomst_id = 94214146, start_date = as_date("2021−05−01"), end_date = as_date("2021−10−24"), probl = 2),
  tibble(site = "EVO031", tomst_id = 94214108, start_date = as_date("2021−05−01"), end_date = as_date("2021−10−19"), probl = 2),
  tibble(site = "EVO031", tomst_id = 94214108, start_date = as_date("2021−10−20"), end_date = as_date("2021−10−24"), probl = 1),
  tibble(site = "EVO031", tomst_id = 94214108, start_date = as_date("2021−10−25"), end_date = as_date("2022−11−23"), probl = 8),
  tibble(site = "EVO032", tomst_id = 94214137, start_date = as_date("2021−05−01"), end_date = as_date("2021−10−25"), probl = 2),
  tibble(site = "EVO033", tomst_id = 94214113, start_date = as_date("2021−05−01"), end_date = as_date("2021−10−19"), probl = 2),
  tibble(site = "EVO033", tomst_id = 94214113, start_date = as_date("2021−10−20"), end_date = as_date("2021−10−24"), probl = 1),
  tibble(site = "EVO033", tomst_id = 94214113, start_date = as_date("2021−10−25"), end_date = as_date("2022−11−23"), probl = 8),
  tibble(site = "EVO034", tomst_id = 94214110, start_date = as_date("2021−05−01"), end_date = as_date("2021−10−19"), probl = 2),
  tibble(site = "EVO034", tomst_id = 94214110, start_date = as_date("2021−10−20"), end_date = as_date("2021−10−24"), probl = 1),
  tibble(site = "EVO034", tomst_id = 94214110, start_date = as_date("2021−10−25"), end_date = as_date("2022−11−23"), probl = 8),
  tibble(site = "EVO036", tomst_id = 94214120, start_date = as_date("2021−05−01"), end_date = as_date("2021−10−19"), probl = 2),
  tibble(site = "EVO036", tomst_id = 94214120, start_date = as_date("2021−10−20"), end_date = as_date("2021−10−21"), probl = 1),
  tibble(site = "EVO036", tomst_id = 94214120, start_date = as_date("2021−10−22"), end_date = as_date("2021−10−25"), probl = 2),
  tibble(site = "EVO041", tomst_id = 94214142, start_date = as_date("2021−05−01"), end_date = as_date("2021−10−24"), probl = 2),
  tibble(site = "EVO043", tomst_id = 94214147, start_date = as_date("2021−05−01"), end_date = as_date("2021−10−24"), probl = 2),
  tibble(site = "EVO043", tomst_id = 94214147, start_date = as_date("2022−07−11"), end_date = as_date("2023−10−25"), probl = 1),
  tibble(site = "EVO052", tomst_id = 94214124, start_date = as_date("2021−05−01"), end_date = as_date("2021−10−03"), probl = 2),
  tibble(site = "EVO052", tomst_id = 94214124, start_date = as_date("2021−10−04"), end_date = as_date("2021−10−21"), probl = 1),
  tibble(site = "EVO052", tomst_id = 94214124, start_date = as_date("2021−10−22"), end_date = as_date("2021−10−25"), probl = 2),
  tibble(site = "EVO053", tomst_id = 94214150, start_date = as_date("2021−05−01"), end_date = as_date("2021−10−19"), probl = 2),
  tibble(site = "EVO053", tomst_id = 94214150, start_date = as_date("2021−10−20"), end_date = as_date("2021−10−21"), probl = 1),
  tibble(site = "EVO053", tomst_id = 94214150, start_date = as_date("2021−10−22"), end_date = as_date("2021−10−24"), probl = 2),
  tibble(site = "EVO054", tomst_id = 94214126, start_date = as_date("2021−05−01"), end_date = as_date("2021−10−25"), probl = 2),
  tibble(site = "EVO056", tomst_id = 94214136, start_date = as_date("2021−05−01"), end_date = as_date("2021−10−25"), probl = 2),
  tibble(site = "EVO057", tomst_id = 94214134, start_date = as_date("2021−05−01"), end_date = as_date("2021−10−25"), probl = 2),
  tibble(site = "EVO060", tomst_id = 94214103, start_date = as_date("2021−05−01"), end_date = as_date("2021−10−19"), probl = 2),
  tibble(site = "EVO060", tomst_id = 94214103, start_date = as_date("2021−10−20"), end_date = as_date("2021−10−24"), probl = 1),
  tibble(site = "EVO060", tomst_id = 94214103, start_date = as_date("2021−10−25"), end_date = as_date("2022−11−22"), probl = 8),
  tibble(site = "EVO061", tomst_id = 94214117, start_date = as_date("2021−05−01"), end_date = as_date("2021−10−19"), probl = 2),
  tibble(site = "EVO061", tomst_id = 94214117, start_date = as_date("2021−10−20"), end_date = as_date("2021−10−21"), probl = 1),
  tibble(site = "EVO061", tomst_id = 94214117, start_date = as_date("2021−10−22"), end_date = as_date("2021−10−25"), probl = 2),
  tibble(site = "EVO064", tomst_id = 94214127, start_date = as_date("2021−05−01"), end_date = as_date("2021−10−25"), probl = 2),
  tibble(site = "EVO064", tomst_id = 94214127, start_date = as_date("2023−10−25"), end_date = as_date("2023−10−27"), probl = 2),
  tibble(site = "EVO065", tomst_id = 94214115, start_date = as_date("2021−05−01"), end_date = as_date("2021−10−19"), probl = 2),
  tibble(site = "EVO065", tomst_id = 94214115, start_date = as_date("2021−10−20"), end_date = as_date("2021−10−24"), probl = 1),
  tibble(site = "EVO065", tomst_id = 94214115, start_date = as_date("2021−10−25"), end_date = as_date("2022−11−22"), probl = 8),
  tibble(site = "EVO066", tomst_id = 94214143, start_date = as_date("2021−05−01"), end_date = as_date("2021−10−24"), probl = 2),
  tibble(site = "EVO067", tomst_id = 94214105, start_date = as_date("2021−05−01"), end_date = as_date("2021−10−19"), probl = 2),
  tibble(site = "EVO067", tomst_id = 94214105, start_date = as_date("2021−10−20"), end_date = as_date("2021−10−24"), probl = 1),
  tibble(site = "EVO067", tomst_id = 94214105, start_date = as_date("2021−10−25"), end_date = as_date("2022−11−22"), probl = 8),
  tibble(site = "EVO067", tomst_id = 94214105, start_date = as_date("2023−09-14"), end_date = as_date("2023−10−25"), probl = 1),
  tibble(site = "EVO072", tomst_id = 94214129, start_date = as_date("2021−05−01"), end_date = as_date("2021−10−25"), probl = 2),
  tibble(site = "EVO074", tomst_id = 94214107, start_date = as_date("2021−05−01"), end_date = as_date("2021−10−19"), probl = 2),
  tibble(site = "EVO074", tomst_id = 94214107, start_date = as_date("2021−10−20"), end_date = as_date("2021−10−24"), probl = 1),
  tibble(site = "EVO074", tomst_id = 94214107, start_date = as_date("2021−10−25"), end_date = as_date("2022−11−23"), probl = 8),
  tibble(site = "EVO076", tomst_id = 94214111, start_date = as_date("2021−05−01"), end_date = as_date("2021−10−19"), probl = 2),
  tibble(site = "EVO076", tomst_id = 94214111, start_date = as_date("2021−10−20"), end_date = as_date("2021−10−24"), probl = 1),
  tibble(site = "EVO076", tomst_id = 94214111, start_date = as_date("2021−10−25"), end_date = as_date("2022−11−23"), probl = 8),
  tibble(site = "EVO077", tomst_id = 94214122, start_date = as_date("2021−05−01"), end_date = as_date("2021−10−19"), probl = 2),
  tibble(site = "EVO077", tomst_id = 94214122, start_date = as_date("2021−10−20"), end_date = as_date("2021−10−21"), probl = 1),
  tibble(site = "EVO077", tomst_id = 94214122, start_date = as_date("2021−10−22"), end_date = as_date("2021−10−25"), probl = 2),
  tibble(site = "EVO078", tomst_id = 94214128, start_date = as_date("2021−05−01"), end_date = as_date("2021−10−25"), probl = 2),
  tibble(site = "EVO079", tomst_id = 94214125, start_date = as_date("2021−05−01"), end_date = as_date("2021−10−19"), probl = 2),
  tibble(site = "EVO079", tomst_id = 94214125, start_date = as_date("2021−10−20"), end_date = as_date("2021−10−21"), probl = 1),
  tibble(site = "EVO079", tomst_id = 94214125, start_date = as_date("2021−10−22"), end_date = as_date("2021−10−25"), probl = 2),
  tibble(site = "EVO080", tomst_id = 94214140, start_date = as_date("2021−05−01"), end_date = as_date("2021−10−25"), probl = 2),
  tibble(site = "EVO081", tomst_id = 94214141, start_date = as_date("2021−05−01"), end_date = as_date("2021−10−24"), probl = 2),
  tibble(site = "EVO082", tomst_id = 94214306, start_date = as_date("2021−05−01"), end_date = as_date("2021−10−19"), probl = 2),
  tibble(site = "EVO082", tomst_id = 94214306, start_date = as_date("2021−10−20"), end_date = as_date("2021−10−24"), probl = 1),
  tibble(site = "EVO082", tomst_id = 94214306, start_date = as_date("2021−10−25"), end_date = as_date("2022−11−23"), probl = 8),
  tibble(site = "EVO085", tomst_id = 94214112, start_date = as_date("2021−05−01"), end_date = as_date("2021−10−19"), probl = 2),
  tibble(site = "EVO085", tomst_id = 94214112, start_date = as_date("2021−10−20"), end_date = as_date("2021−10−24"), probl = 1),
  tibble(site = "EVO085", tomst_id = 94214112, start_date = as_date("2021−10−25"), end_date = as_date("2022−11−23"), probl = 8),
  tibble(site = "EVO087", tomst_id = 94214132, start_date = as_date("2021−05−01"), end_date = as_date("2021−10−25"), probl = 2),
  tibble(site = "EVO088", tomst_id = 94214149, start_date = as_date("2021−05−01"), end_date = as_date("2021−10−24"), probl = 2),
  tibble(site = "EVO090", tomst_id = 94214109, start_date = as_date("2021−05−01"), end_date = as_date("2021−10−19"), probl = 2),
  tibble(site = "EVO090", tomst_id = 94214109, start_date = as_date("2021−10−20"), end_date = as_date("2021−10−24"), probl = 1),
  tibble(site = "EVO090", tomst_id = 94214109, start_date = as_date("2021−10−25"), end_date = as_date("2022−11−23"), probl = 8),
  tibble(site = "EVO093", tomst_id = 94214139, start_date = as_date("2021−05−01"), end_date = as_date("2021−10−25"), probl = 2),
  tibble(site = "EVO094", tomst_id = 94214106, start_date = as_date("2021−05−01"), end_date = as_date("2021−10−19"), probl = 2),
  tibble(site = "EVO094", tomst_id = 94214106, start_date = as_date("2021−10−20"), end_date = as_date("2021−10−21"), probl = 1),
  tibble(site = "EVO094", tomst_id = 94214106, start_date = as_date("2021−10−22"), end_date = as_date("2021−10−24"), probl = 2),
  tibble(site = "EVO095", tomst_id = 94214131, start_date = as_date("2021−05−01"), end_date = as_date("2021−10−25"), probl = 2),
  tibble(site = "EVO097", tomst_id = 94214114, start_date = as_date("2021−05−01"), end_date = as_date("2021−10−19"), probl = 2),
  tibble(site = "EVO097", tomst_id = 94214114, start_date = as_date("2021−10−20"), end_date = as_date("2021−10−24"), probl = 1),
  tibble(site = "EVO097", tomst_id = 94214114, start_date = as_date("2021−10−25"), end_date = as_date("2022−11−23"), probl = 8),
  tibble(site = "EVO098", tomst_id = 94214138, start_date = as_date("2021−05−01"), end_date = as_date("2021−10−25"), probl = 2),
  tibble(site = "EVO100", tomst_id = 94214119, start_date = as_date("2021−05−01"), end_date = as_date("2021−10−19"), probl = 2),
  tibble(site = "EVO100", tomst_id = 94214119, start_date = as_date("2021−10−20"), end_date = as_date("2021−10−21"), probl = 1),
  tibble(site = "EVO100", tomst_id = 94214119, start_date = as_date("2021−10−22"), end_date = as_date("2021−10−25"), probl = 2),
  tibble(site = "EVO101", tomst_id = 94214130, start_date = as_date("2021−05−01"), end_date = as_date("2021−10−25"), probl = 2)
)

# el <- bind_rows(el,
#                 nel) %>% 
#   unique()
el <- nel

sites[!sites %in% nel$site]
unique(df$tomst_id)[!unique(df$tomst_id) %in% nel$tomst_id]

el %>% write_csv(paste0("data/",area,"/error_log_",area,".csv"))

el$date <- apply(el, 1, function(x){
  seq.Date(from = as_date(x["start_date"]), to = as_date(x["end_date"]), by = 1)
})

el <- el %>% 
  unnest(date) %>% 
  select(-start_date, -end_date)

df <- left_join(df %>% mutate(date = as_date(datetime)), el) %>% 
  mutate(probl = ifelse(is.na(probl), 0, probl)) %>% 
  select(-date)

########################################################################
# FILL MISSING TIMESTAMPS WITH NA

tids <- unique(df$tomst_id)

df <- lapply(tids, fill_timestamps, df = df) %>% 
  bind_rows()

#################################################################################
# CORRECT BIASES BASED ON THE NOT-IN-FIELD DATA
#

ctd <- read_csv("data/Correction_temperatures.csv")
# ctd <- ctd %>% filter(!tomst_id %in% unique(df$tomst_id))
# write_csv(ctd, "data/Correction_temperatures.csv")

# See if all tomsts devices already have correction values calculated
tomsts_to_cor <- unique(df$tomst_id)[!unique(df$tomst_id) %in% ctd$tomst_id]
length(tomsts_to_cor)
# All are there

if(length(tomsts_to_cor) > 0){
  # tomsts_to_cor <- c(94190043, 94212877)
  diffs_all <- correct_tomst_cross_sensor(tomsts_to_cor, df)
  
  diffs_all <- bind_rows(ctd, diffs_all) %>% 
    drop_na()
  
  fwrite(diffs_all, "data/Correction_temperatures.csv")
  
}

###################################################################################
# Delete erroneous data
#

# Delete not-in-field data
df %>% filter(probl != 2) -> df

# Look for weird extra measurements between real ones
df <- df %>% 
  mutate(mins = minute(datetime))

for(i in unique(df$tomst_id)){
  print(i)
  
  td <- df %>% 
    filter(tomst_id == i)
  
  tb <- table(td$mins)/nrow(td)
  tb <- tb[tb < 0.0001]
  if(sum(tb)*nrow(td) > 0){
    print(paste0("Removing ", sum(tb)*nrow(td), " rows..."))
  }
  
  df <- df %>% 
    filter(!(tomst_id == i & mins %in% as.numeric(names(tb))))
  
}

# ###############################################################################
# # Combine with data from previous years
# 
# # Mark temporarily this as new data
# df <- df %>% 
#   mutate(datanew = TRUE) %>% 
#   select(-zone,-mins) %>% 
#   rename(error_tomst = probl)
# 
# # Combine with previous years Tomst data
# od <- fread(paste0(old_data_dir,"/tomst_data_raw.csv")) %>%
#   mutate(datetime = with_tz(datetime, tzone = "Etc/GMT-2")) %>% 
#   mutate(site = add_zeros(site, area)) %>% 
#   mutate(datanew = FALSE) %>% 
#   filter(!(is.na(T1) | is.na(T2) | is.na(T3) | is.na(moist)))
# 
# df <- bind_rows(od,df) %>% 
#   arrange(site, datetime)

########################################################################
# FILL MISSING TIMESTAMPS WITH NA

df <- df %>% 
  select(-mins, -zone) %>% 
  rename(error_tomst = probl)

tids <- unique(df$site)

df <- lapply(tids, fill_timestamps_site, df = df) %>% 
  bind_rows()

df <- df %>% 
  distinct(site, datetime, .keep_all = T)


pdf(paste0(temp_dir,"/Temperature_graphs.pdf"), 15, 10)
for(i in unique(df$site) %>% sort){
  # i <- "VAR041"
  print(i)
  df %>% filter(site == i) %>% 
    mutate(T1 = as.numeric(ifelse(error_tomst %in% c(1,4,9), NA, T1))) %>% 
    mutate(T2 = as.numeric(ifelse(error_tomst %in% c(1,7,8), NA, T2))) %>% 
    mutate(T3 = as.numeric(ifelse(error_tomst %in% c(1,4,5,7,8,10), NA, T3))) %>% 
    #group_by(date) %>% 
    #summarise_at(vars(i, "soil"), funs(mean, min, max), na.rm = T) %>% 
    #lapply(function(x) replace(x, is.infinite(x),NA)) %>% as_tibble() %>% 
    ggplot(aes(x=datetime)) +
    geom_line(aes(y = T3), col = "cornflowerblue") +
    geom_line(aes(y = T2), col = "brown1") +
    geom_line(aes(y = T1), col = "darkgoldenrod") +
    theme_minimal() +
    ylab("Temperature") + xlab("Date")+
    # scale_y_continuous(limits = c(-30, 35))+
    ggtitle(i) -> GG1
  
  df %>% filter(site == i) %>% 
    mutate(moist = as.numeric(ifelse(error_tomst %in% c(1,6,8,9,10), NA, moist))) %>% 
    mutate(moist = as.numeric(ifelse(T1 <= 1, NA, moist))) %>% 
    #group_by(date) %>% 
    #summarise_at(vars(i, "soil"), funs(mean, min, max), na.rm = T) %>% 
    #lapply(function(x) replace(x, is.infinite(x),NA)) %>% as_tibble() %>% 
    ggplot(aes(x=datetime)) +
    geom_line(aes(y = moist), col = "black") +
    theme_minimal() +
    ylab("Soil moisture count") + xlab("Date")+
    # scale_y_continuous(limits = c(500, 4000))+
    ggtitle(i) -> GG2
  
  print(plot_grid(plotlist = list(GG1, GG2), nrow = 2))
  
}
dev.off()

fwrite(df, paste0("output/",area,"/tomst_data_raw.csv"))

####################################################################################