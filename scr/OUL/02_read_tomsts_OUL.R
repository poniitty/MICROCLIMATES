library(tidyverse)
library(lubridate)
library(data.table)
library(cowplot)
library(zoo)

area <- "OUL"

invisible(lapply(list.files("scr/functions/", ".R$", full.names = T), source))

# Set date limits to remove implausible dates
mind <- as.Date("2018-06-01", tz = "Etc/GMT-2")
maxd <- as.Date("2023-10-04", tz = "Etc/GMT-2")

raw_data_dir <- "/scratch/project_2007415/microclim/2023_data/OUL"
# old_data_dir <- "/projappl/project_2003061/repos/microclim_rastigaisa/output"
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
  tibble(site = "OUL001", tomst_id = 94217025, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−04"), probl = 2),
  tibble(site = "OUL002", tomst_id = 94217031, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−04"), probl = 2),
  tibble(site = "OUL003", tomst_id = 94212882, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−04"), probl = 2),
  tibble(site = "OUL003", tomst_id = 94212882, start_date = as_date("2023−05−26"), end_date = as_date("2023−09−24"), probl = 1),
  tibble(site = "OUL004", tomst_id = 94217075, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−04"), probl = 2),
  tibble(site = "OUL005", tomst_id = 94213538, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−04"), probl = 2),
  tibble(site = "OUL006", tomst_id = 94212855, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−04"), probl = 2),
  tibble(site = "OUL007", tomst_id = 94212890, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−04"), probl = 2),
  tibble(site = "OUL008", tomst_id = 94212851, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−04"), probl = 2),
  tibble(site = "OUL009", tomst_id = 94217064, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−04"), probl = 2),
  tibble(site = "OUL010", tomst_id = 94212891, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−04"), probl = 2),
  tibble(site = "OUL011", tomst_id = 94213502, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−04"), probl = 2),
  tibble(site = "OUL012", tomst_id = 94217040, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−04"), probl = 2),
  tibble(site = "OUL013", tomst_id = 94217068, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−04"), probl = 2),
  tibble(site = "OUL014", tomst_id = 94217021, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−04"), probl = 2),
  tibble(site = "OUL015", tomst_id = 94212868, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−04"), probl = 2),
  tibble(site = "OUL016", tomst_id = 94213540, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−04"), probl = 2),
  tibble(site = "OUL017", tomst_id = 94217047, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−04"), probl = 2),
  tibble(site = "OUL018", tomst_id = 94212875, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−05"), probl = 2),
  tibble(site = "OUL019", tomst_id = 94217041, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−05"), probl = 2),
  tibble(site = "OUL020", tomst_id = 94213536, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−05"), probl = 2),
  tibble(site = "OUL021", tomst_id = 94212889, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−05"), probl = 2),
  tibble(site = "OUL022", tomst_id = 94217015, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−05"), probl = 2),
  tibble(site = "OUL023", tomst_id = 94217058, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−05"), probl = 2),
  tibble(site = "OUL024", tomst_id = 94217061, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−05"), probl = 2),
  tibble(site = "OUL025", tomst_id = 94213549, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−05"), probl = 2),
  tibble(site = "OUL026", tomst_id = 94217001, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−05"), probl = 2),
  tibble(site = "OUL027", tomst_id = 94212871, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−05"), probl = 2),
  tibble(site = "OUL028", tomst_id = 94217006, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−05"), probl = 2),
  tibble(site = "OUL029", tomst_id = 94217056, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−05"), probl = 2),
  tibble(site = "OUL029", tomst_id = 94217056, start_date = as_date("2023−06−07"), end_date = as_date("2023−09−24"), probl = 1),
  tibble(site = "OUL030", tomst_id = 94217035, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−05"), probl = 2),
  tibble(site = "OUL031", tomst_id = 94217020, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−05"), probl = 2),
  tibble(site = "OUL032", tomst_id = 94204789, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−05"), probl = 2),
  tibble(site = "OUL033", tomst_id = 94217019, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−05"), probl = 2),
  tibble(site = "OUL034", tomst_id = 94213545, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−05"), probl = 2),
  tibble(site = "OUL035", tomst_id = 94217007, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−05"), probl = 2),
  tibble(site = "OUL036", tomst_id = 94217071, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−05"), probl = 2),
  tibble(site = "OUL037", tomst_id = 95223545, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−07"), probl = 2),
  tibble(site = "OUL038", tomst_id = 95223544, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−07"), probl = 2),
  tibble(site = "OUL039", tomst_id = 95223543, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−07"), probl = 2),
  tibble(site = "OUL040", tomst_id = 95223541, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−07"), probl = 2),
  tibble(site = "OUL041", tomst_id = 95223542, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−07"), probl = 2),
  tibble(site = "OUL042", tomst_id = 95223520, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−07"), probl = 2),
  tibble(site = "OUL043", tomst_id = 95223519, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−07"), probl = 2),
  tibble(site = "OUL044", tomst_id = 95223517, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−07"), probl = 2),
  tibble(site = "OUL045", tomst_id = 95223518, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−07"), probl = 2),
  tibble(site = "OUL046", tomst_id = 95223516, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−07"), probl = 2),
  tibble(site = "OUL047", tomst_id = 94213527, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−07"), probl = 2),
  tibble(site = "OUL048", tomst_id = 94213543, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−07"), probl = 2),
  tibble(site = "OUL049", tomst_id = 94213504, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−07"), probl = 2),
  tibble(site = "OUL050", tomst_id = 94217010, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−07"), probl = 2),
  tibble(site = "OUL052", tomst_id = 94213528, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−07"), probl = 2),
  tibble(site = "OUL053", tomst_id = 94213544, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−07"), probl = 2),
  tibble(site = "OUL054", tomst_id = 94212885, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−07"), probl = 2),
  tibble(site = "OUL055", tomst_id = 94213513, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−07"), probl = 2),
  tibble(site = "OUL056", tomst_id = 94212869, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−07"), probl = 2),
  tibble(site = "OUL056", tomst_id = 94212869, start_date = as_date("2023−06−17"), end_date = as_date("2023−09−25"), probl = 1),
  tibble(site = "OUL057", tomst_id = 95223525, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−08"), probl = 2),
  tibble(site = "OUL058", tomst_id = 95223513, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−08"), probl = 2),
  tibble(site = "OUL059", tomst_id = 95223515, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−08"), probl = 2),
  tibble(site = "OUL060", tomst_id = 95223511, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−08"), probl = 2),
  tibble(site = "OUL061", tomst_id = 95223514, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−08"), probl = 2),
  tibble(site = "OUL062", tomst_id = 95223512, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−08"), probl = 2),
  tibble(site = "OUL063", tomst_id = 95223521, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−08"), probl = 2),
  tibble(site = "OUL064", tomst_id = 95223524, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−08"), probl = 2),
  tibble(site = "OUL065", tomst_id = 95223523, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−08"), probl = 2),
  tibble(site = "OUL066", tomst_id = 95223505, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−09"), probl = 2),
  tibble(site = "OUL067", tomst_id = 95223522, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−09"), probl = 2),
  tibble(site = "OUL068", tomst_id = 95223510, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−09"), probl = 2),
  tibble(site = "OUL069", tomst_id = 95223508, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−09"), probl = 2),
  tibble(site = "OUL070", tomst_id = 95223507, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−09"), probl = 2),
  tibble(site = "OUL071", tomst_id = 95223501, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−09"), probl = 2),
  tibble(site = "OUL072", tomst_id = 95223506, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−09"), probl = 2),
  tibble(site = "OUL073", tomst_id = 95223509, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−09"), probl = 2),
  tibble(site = "OUL074", tomst_id = 95223503, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−09"), probl = 2),
  tibble(site = "OUL075", tomst_id = 95223504, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−09"), probl = 2),
  tibble(site = "OUL076", tomst_id = 95223502, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−09"), probl = 2),
  tibble(site = "OUL077", tomst_id = 95134862, start_date = as_date("2021−04−01"), end_date = as_date("2023−09−27"), probl = 2),
  tibble(site = "OUL078", tomst_id = 95223552, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−09"), probl = 2),
  tibble(site = "OUL079", tomst_id = 95223547, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−10"), probl = 2),
  tibble(site = "OUL080", tomst_id = 95223551, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−10"), probl = 2),
  tibble(site = "OUL081", tomst_id = 95223548, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−10"), probl = 2),
  tibble(site = "OUL082", tomst_id = 95223536, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−10"), probl = 2),
  tibble(site = "OUL083", tomst_id = 95223537, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−10"), probl = 2),
  tibble(site = "OUL084", tomst_id = 95223546, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−10"), probl = 2),
  tibble(site = "OUL085", tomst_id = 95223553, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−10"), probl = 2),
  tibble(site = "OUL086", tomst_id = 95223540, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−10"), probl = 2),
  tibble(site = "OUL087", tomst_id = 95223550, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−10"), probl = 2),
  tibble(site = "OUL088", tomst_id = 95223555, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−10"), probl = 2),
  tibble(site = "OUL089", tomst_id = 95223538, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−10"), probl = 2),
  tibble(site = "OUL090", tomst_id = 95223539, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−10"), probl = 2),
  tibble(site = "OUL091", tomst_id = 95223554, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−10"), probl = 2),
  tibble(site = "OUL092", tomst_id = 95223534, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−11"), probl = 2),
  tibble(site = "OUL093", tomst_id = 95223532, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−11"), probl = 2),
  tibble(site = "OUL094", tomst_id = 95223529, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−11"), probl = 2),
  tibble(site = "OUL095", tomst_id = 95223526, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−11"), probl = 2),
  tibble(site = "OUL096", tomst_id = 95223531, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−11"), probl = 2),
  tibble(site = "OUL097", tomst_id = 95223530, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−11"), probl = 2),
  tibble(site = "OUL098", tomst_id = 95223535, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−11"), probl = 2),
  tibble(site = "OUL099", tomst_id = 94212857, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−11"), probl = 2),
  tibble(site = "OUL100", tomst_id = 95223533, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−11"), probl = 2),
  tibble(site = "OUL101", tomst_id = 95223528, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−12"), probl = 2),
  tibble(site = "OUL102", tomst_id = 94212880, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−12"), probl = 2),
  tibble(site = "OUL103", tomst_id = 95223527, start_date = as_date("2021−04−01"), end_date = as_date("2022−10−12"), probl = 2)
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

# See if all tomsts devices already have correction values calculated
tomsts_to_cor <- unique(df$tomst_id)[!unique(df$tomst_id) %in% ctd$tomst_id]
length(tomsts_to_cor)
# No previous correction factors

if(length(tomsts_to_cor) > 0){
  # tomsts_to_cor <- c(94190043, 94212877)
  diffs_all <- correct_tomst_cross_sensor(tomsts_to_cor, df)
  
  diffs_all <- bind_rows(ctd, diffs_all)
  
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