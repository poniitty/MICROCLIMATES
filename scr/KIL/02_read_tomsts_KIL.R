library(tidyverse)
library(lubridate)
library(data.table)
library(cowplot)
library(zoo)

area <- "KIL"

invisible(lapply(list.files("scr/functions/", ".R$", full.names = T), source))

# Set date limits to remove implausible dates
mind <- as.Date("2021-04-01", tz = "Etc/GMT-2")
maxd <- as.Date("2023-10-01", tz = "Etc/GMT-2")

raw_data_dir <- "/scratch/project_2007415/microclim/2023_data/KIL"
old_data_dir <- "/projappl/project_2003061/repos/microclim_kilpisjarvi/output/"
temp_dir <- "/scratch/project_2007415/temp/"

#################################################################################3
# Read earlier visiting/reading times

visittimes <- read_csv(paste0("data/",area,"/reading_times_",area,".csv")) %>%
  mutate(maxdt = with_tz(maxdt, tzone = "Etc/GMT-2")) %>% 
  mutate(site = ifelse(site == "MI019", "MI19", site))

logids <- read_csv(paste0("data/",area,"/site_vs_tomst_ids_2023.csv")) %>% 
  mutate(site = ifelse(site == "X2", "X02", site))

# List logger data files to read
f <- list.files(raw_data_dir, pattern = "data_", full.names = T, recursive = T)

df <- lapply(f, read_tomst_data) %>% 
  rbindlist()

df <- df %>% 
  distinct(tomst_id, datetime, .keep_all = T) %>% 
  arrange(tomst_id, datetime)

df <- left_join(df,
                logids,
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

sites <- unique(df$site) %>% sort

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

# Read the previous error log
el <- read_csv(paste0("data/",area,"/error_log_",area,".csv"))

unique(df$site)[!unique(df$site) %in% el$site]
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
  tibble(site = "AE01", tomst_id = 95256186, start_date = as_date("2022-06-01"), end_date = as_date("2023-07-09"), probl = 2),
  tibble(site = "AE03", tomst_id = 95256175, start_date = as_date("2022-06-01"), end_date = as_date("2023-07-09"), probl = 2),
  tibble(site = "AE04", tomst_id = 95256174, start_date = as_date("2022-06-01"), end_date = as_date("2023-07-09"), probl = 2),
  tibble(site = "AE05", tomst_id = 95256173, start_date = as_date("2022-06-01"), end_date = as_date("2023-07-09"), probl = 2),
  tibble(site = "AE06", tomst_id = 95256172, start_date = as_date("2022-06-01"), end_date = as_date("2023-07-09"), probl = 2),
  tibble(site = "AE07", tomst_id = 95256171, start_date = as_date("2022-06-01"), end_date = as_date("2023-07-09"), probl = 2),
  tibble(site = "AE08", tomst_id = 95256185, start_date = as_date("2022-06-01"), end_date = as_date("2023-07-09"), probl = 2),
  tibble(site = "AE09", tomst_id = 95256184, start_date = as_date("2022-06-01"), end_date = as_date("2023-07-09"), probl = 2),
  tibble(site = "AE10", tomst_id = 95256183, start_date = as_date("2022-06-01"), end_date = as_date("2023-07-09"), probl = 2),
  tibble(site = "AE11", tomst_id = 95256182, start_date = as_date("2022-06-01"), end_date = as_date("2023-07-09"), probl = 2),
  tibble(site = "AE12", tomst_id = 95256181, start_date = as_date("2022-06-01"), end_date = as_date("2023-07-09"), probl = 2),
  tibble(site = "AIL119", tomst_id = 94194026, start_date = as_date("2023-04-19"), end_date = as_date("2023-09-07"), probl = 1),
  tibble(site = "AIL127", tomst_id = 94194022, start_date = as_date("2023−09−07"), end_date = as_date("2023-09-07"), probl = 1),
  tibble(site = "AIL128", tomst_id = 94194021, start_date = as_date("2023−09−07"), end_date = as_date("2023-09-07"), probl = 1),
  tibble(site = "AIL129", tomst_id = 94194023, start_date = as_date("2022−09−28"), end_date = as_date("2023-09-07"), probl = 1),
  tibble(site = "AIL188", tomst_id = 94194016, start_date = as_date("2022−09−23"), end_date = as_date("2023-09-12"), probl = 1),
  tibble(site = "L11", tomst_id = 94217045, start_date = as_date("2019−09−23"), end_date = as_date("2022-09-21"), probl = 2),
  tibble(site = "L12", tomst_id = 94217052, start_date = as_date("2019−09−23"), end_date = as_date("2022-09-20"), probl = 2),
  tibble(site = "L14", tomst_id = 94212881, start_date = as_date("2019−09−23"), end_date = as_date("2022-09-21"), probl = 2),
  tibble(site = "L15", tomst_id = 94213516, start_date = as_date("2021-05-13"), end_date = as_date("2021-07-30"), probl = 2),
  tibble(site = "L15", tomst_id = 94213516, start_date = as_date("2022-06-18"), end_date = as_date("2022-07-19"), probl = 9),
  tibble(site = "L15", tomst_id = 94217022, start_date = as_date("2020-01-01"), end_date = as_date("2022-08-20"), probl = 2),
  tibble(site = "L26", tomst_id = 94213507, start_date = as_date("2020-01-01"), end_date = as_date("2022-07-30"), probl = 2),
  tibble(site = "L27", tomst_id = 94213519, start_date = as_date("2020-01-01"), end_date = as_date("2022-07-30"), probl = 2),
  tibble(site = "L28", tomst_id = 94213539, start_date = as_date("2020-01-01"), end_date = as_date("2022-07-30"), probl = 2),
  tibble(site = "L29", tomst_id = 94217062, start_date = as_date("2020-01-01"), end_date = as_date("2022-07-30"), probl = 2),
  tibble(site = "L30", tomst_id = 94213541, start_date = as_date("2020-01-01"), end_date = as_date("2022-07-30"), probl = 2),
  tibble(site = "L31", tomst_id = 94217043, start_date = as_date("2020-01-01"), end_date = as_date("2022-07-30"), probl = 2),
  tibble(site = "L31", tomst_id = 94217043, start_date = as_date("2023-07-13"), end_date = as_date("2023-09-06"), probl = 1),
  tibble(site = "L32", tomst_id = 94217070, start_date = as_date("2020-01-01"), end_date = as_date("2022-07-30"), probl = 2),
  tibble(site = "L33", tomst_id = 94212874, start_date = as_date("2020-01-01"), end_date = as_date("2022-09-20"), probl = 2),
  tibble(site = "L34", tomst_id = 94217072, start_date = as_date("2020-01-01"), end_date = as_date("2022-07-30"), probl = 2),
  tibble(site = "L35", tomst_id = 94217049, start_date = as_date("2020-01-01"), end_date = as_date("2022-08-14"), probl = 2),
  tibble(site = "L28", tomst_id = 94213507, start_date = as_date("2020-01-01"), end_date = as_date("2022-07-30"), probl = 2),
  tibble(site = "L6", tomst_id = 94213508, start_date = as_date("2023-02-13"), end_date = as_date("2023-09-07"), probl = 1),
  tibble(site = "MAL013", tomst_id = 94194088, start_date = as_date("2023-08-31"), end_date = as_date("2023-09-10"), probl = 1),
  tibble(site = "MAL015", tomst_id = 94194061, start_date = as_date("2023-08-05"), end_date = as_date("2023-09-10"), probl = 1),
  tibble(site = "MAL021", tomst_id = 94194084, start_date = as_date("2023-08-31"), end_date = as_date("2023-09-10"), probl = 1),
  tibble(site = "MAL022", tomst_id = 94194089, start_date = as_date("2023-08-07"), end_date = as_date("2023-09-10"), probl = 1),
  tibble(site = "MAL037", tomst_id = 94194151, start_date = as_date("2023-08-09"), end_date = as_date("2023-09-10"), probl = 1),
  tibble(site = "MAL084", tomst_id = 94194113, start_date = as_date("2023-07-23"), end_date = as_date("2023-09-14"), probl = 1),
  tibble(site = "MI1043", tomst_id = 94217055, start_date = as_date("2020-01-01"), end_date = as_date("2022-09-18"), probl = 2),
  tibble(site = "MI11231", tomst_id = 94217016, start_date = as_date("2020-01-01"), end_date = as_date("2022-09-20"), probl = 2),
  tibble(site = "MI11232", tomst_id = 94181614, start_date = as_date("2023-08-22"), end_date = as_date("2023-09-04"), probl = 6),
  tibble(site = "MI343", tomst_id = 94181308, start_date = as_date("2022-09-19"), end_date = as_date("2023-09-03"), probl = 6),
  tibble(site = "MI367", tomst_id = 94212870, start_date = as_date("2020-01-01"), end_date = as_date("2022-09-18"), probl = 2),
  tibble(site = "MI583", tomst_id = 94213503, start_date = as_date("2020-01-01"), end_date = as_date("2022-09-19"), probl = 2),
  tibble(site = "MI751", tomst_id = 94217018, start_date = as_date("2020-01-01"), end_date = as_date("2022-09-19"), probl = 2),
  tibble(site = "MI907", tomst_id = 94181601, start_date = as_date("2023-06-06"), end_date = as_date("2023-06-16"), probl = 1),
  tibble(site = "MI949", tomst_id = 94181608, start_date = as_date("2023-08-08"), end_date = as_date("2023-09-06"), probl = 1),
  tibble(site = "RA300", tomst_id = 94204779, start_date = as_date("2020-01-01"), end_date = as_date("2022-08-01"), probl = 2),
  tibble(site = "RA301", tomst_id = 94204781, start_date = as_date("2020-01-01"), end_date = as_date("2022-08-01"), probl = 2),
  tibble(site = "RA302", tomst_id = 94204783, start_date = as_date("2020-01-01"), end_date = as_date("2022-08-01"), probl = 2),
  tibble(site = "RA303", tomst_id = 94204749, start_date = as_date("2020-01-01"), end_date = as_date("2022-08-01"), probl = 2),
  tibble(site = "RA304", tomst_id = 94204728, start_date = as_date("2020-01-01"), end_date = as_date("2022-08-01"), probl = 2),
  tibble(site = "RA305", tomst_id = 94204735, start_date = as_date("2020-01-01"), end_date = as_date("2022-08-09"), probl = 2),
  tibble(site = "RA305", tomst_id = 94204735, start_date = as_date("2023-07-20"), end_date = as_date("2023-09-30"), probl = 2),
  tibble(site = "RA308", tomst_id = 94204729, start_date = as_date("2020-01-01"), end_date = as_date("2022-08-12"), probl = 2),
  tibble(site = "RA309", tomst_id = 94204744, start_date = as_date("2020-01-01"), end_date = as_date("2022-08-12"), probl = 2),
  tibble(site = "RA310", tomst_id = 94204743, start_date = as_date("2020-01-01"), end_date = as_date("2022-08-12"), probl = 2),
  tibble(site = "RA310", tomst_id = 94204743, start_date = as_date("2023-06-18"), end_date = as_date("2023-09-04"), probl = 1),
  tibble(site = "RA311", tomst_id = 94204741, start_date = as_date("2020-01-01"), end_date = as_date("2022-08-12"), probl = 2),
  tibble(site = "RA312", tomst_id = 94204724, start_date = as_date("2020-01-01"), end_date = as_date("2022-08-13"), probl = 2),
  tibble(site = "RA312", tomst_id = 94204724, start_date = as_date("2023-07-12"), end_date = as_date("2023-09-30"), probl = 2),
  tibble(site = "RA313", tomst_id = 94204764, start_date = as_date("2020-01-01"), end_date = as_date("2022-08-13"), probl = 2),
  tibble(site = "RA313", tomst_id = 94204764, start_date = as_date("2022-09-19"), end_date = as_date("2023−06−01"), probl = 6),
  tibble(site = "RA313", tomst_id = 94204764, start_date = as_date("2023-07-12"), end_date = as_date("2023-09-30"), probl = 2),
  tibble(site = "RA314", tomst_id = 94204721, start_date = as_date("2020-01-01"), end_date = as_date("2022-08-13"), probl = 2),
  tibble(site = "RA314", tomst_id = 94204721, start_date = as_date("2023-07-12"), end_date = as_date("2023-09-30"), probl = 2),
  tibble(site = "RA315", tomst_id = 94204718, start_date = as_date("2020-01-01"), end_date = as_date("2022-08-13"), probl = 2),
  tibble(site = "RA315", tomst_id = 94204718, start_date = as_date("2020-01-01"), end_date = as_date("2022-08-13"), probl = 2),
  tibble(site = "RA316", tomst_id = 94204765, start_date = as_date("2020-01-01"), end_date = as_date("2022-08-13"), probl = 2),
  tibble(site = "RA316", tomst_id = 94204765, start_date = as_date("2023-07-12"), end_date = as_date("2023-09-30"), probl = 2),
  tibble(site = "RA317", tomst_id = 94204763, start_date = as_date("2020-01-01"), end_date = as_date("2022-08-13"), probl = 2),
  tibble(site = "RA317", tomst_id = 94204763, start_date = as_date("2023-07-12"), end_date = as_date("2023-09-30"), probl = 2),
  tibble(site = "RA318", tomst_id = 94204714, start_date = as_date("2020-01-01"), end_date = as_date("2022-08-13"), probl = 2),
  tibble(site = "RA318", tomst_id = 94204714, start_date = as_date("2023-07-12"), end_date = as_date("2023-09-30"), probl = 2),
  tibble(site = "RA319", tomst_id = 94204801, start_date = as_date("2020-01-01"), end_date = as_date("2022-08-13"), probl = 2),
  tibble(site = "RA320", tomst_id = 94204799, start_date = as_date("2020-01-01"), end_date = as_date("2022-08-16"), probl = 2),
  tibble(site = "RA320", tomst_id = 94204799, start_date = as_date("2022-08-17"), end_date = as_date("2023-09-05"), probl = 9),
  tibble(site = "RA321", tomst_id = 94204716, start_date = as_date("2020-01-01"), end_date = as_date("2022-08-16"), probl = 2),
  tibble(site = "RA322", tomst_id = 94204715, start_date = as_date("2020-01-01"), end_date = as_date("2022-08-16"), probl = 2),
  tibble(site = "RA323", tomst_id = 94204738, start_date = as_date("2020-01-01"), end_date = as_date("2022-08-16"), probl = 2),
  tibble(site = "RA323", tomst_id = 94204738, start_date = as_date("2023-07-20"), end_date = as_date("2023-09-30"), probl = 2),
  tibble(site = "SAL007", tomst_id = 95256189, start_date = as_date("2023-06-01"), end_date = as_date("2023-07-11"), probl = 2),
  tibble(site = "SAL008", tomst_id = 95256190, start_date = as_date("2023-06-01"), end_date = as_date("2023-07-11"), probl = 2),
  tibble(site = "SAL009", tomst_id = 95256165, start_date = as_date("2023-06-01"), end_date = as_date("2023-07-11"), probl = 2),
  tibble(site = "SAL018", tomst_id = 95256151, start_date = as_date("2023-06-01"), end_date = as_date("2023-07-11"), probl = 2),
  tibble(site = "SAL019", tomst_id = 95256160, start_date = as_date("2023-06-01"), end_date = as_date("2023-07-11"), probl = 2),
  tibble(site = "SAL020", tomst_id = 95256159, start_date = as_date("2023-06-01"), end_date = as_date("2023-07-11"), probl = 2),
  tibble(site = "SAL021", tomst_id = 95256158, start_date = as_date("2023-06-01"), end_date = as_date("2023-07-11"), probl = 2),
  tibble(site = "SAL021", tomst_id = 95256158, start_date = as_date("2023-08-03"), end_date = as_date("2023-09-09"), probl = 1),
  tibble(site = "SAL023", tomst_id = 95256157, start_date = as_date("2023-06-01"), end_date = as_date("2023-07-11"), probl = 2),
  tibble(site = "SAL024", tomst_id = 95256156, start_date = as_date("2023-06-01"), end_date = as_date("2023-07-11"), probl = 2),
  tibble(site = "SAL025", tomst_id = 95256198, start_date = as_date("2023-06-01"), end_date = as_date("2023-07-08"), probl = 2),
  tibble(site = "SAL026", tomst_id = 95256200, start_date = as_date("2023-06-01"), end_date = as_date("2023-07-08"), probl = 2),
  tibble(site = "SE01", tomst_id = 95256195, start_date = as_date("2023-06-01"), end_date = as_date("2023-07-08"), probl = 2),
  tibble(site = "SE02", tomst_id = 95256194, start_date = as_date("2023-06-01"), end_date = as_date("2023-07-08"), probl = 2),
  tibble(site = "SE03", tomst_id = 95256193, start_date = as_date("2023-06-01"), end_date = as_date("2023-07-08"), probl = 2),
  tibble(site = "SE04", tomst_id = 95256192, start_date = as_date("2023-06-01"), end_date = as_date("2023-07-08"), probl = 2),
  tibble(site = "SE05", tomst_id = 95256191, start_date = as_date("2023-06-01"), end_date = as_date("2023-07-08"), probl = 2),
  tibble(site = "SE06", tomst_id = 95256180, start_date = as_date("2023-06-01"), end_date = as_date("2023-07-08"), probl = 2),
  tibble(site = "SE07", tomst_id = 95256179, start_date = as_date("2023-06-01"), end_date = as_date("2023-07-08"), probl = 2),
  tibble(site = "SE08", tomst_id = 95256178, start_date = as_date("2023-06-01"), end_date = as_date("2023-07-08"), probl = 2),
  tibble(site = "SE09", tomst_id = 95256177, start_date = as_date("2023-06-01"), end_date = as_date("2023-07-08"), probl = 2),
  tibble(site = "SE10", tomst_id = 95256176, start_date = as_date("2023-06-01"), end_date = as_date("2023-07-08"), probl = 2),
  tibble(site = "SE11", tomst_id = 95256196, start_date = as_date("2023-06-01"), end_date = as_date("2023-07-08"), probl = 2),
  tibble(site = "SE12", tomst_id = 95256197, start_date = as_date("2023-06-01"), end_date = as_date("2023-07-08"), probl = 2),
  tibble(site = "SE12", tomst_id = 95256197, start_date = as_date("2023-06-01"), end_date = as_date("2023-07-08"), probl = 2),
  tibble(site = "X01", tomst_id = 94213542, start_date = as_date("2020-01-01"), end_date = as_date("2022−09−22"), probl = 2),
  tibble(site = "X02", tomst_id = 94212860, start_date = as_date("2020-01-01"), end_date = as_date("2022−09−22"), probl = 2),
  tibble(site = "X02", tomst_id = 94212860, start_date = as_date("2023-08-17"), end_date = as_date("2023−09−01"), probl = 1),
  tibble(site = "X04", tomst_id = 94213530, start_date = as_date("2020-01-01"), end_date = as_date("2022−09−22"), probl = 2),
  tibble(site = "X05", tomst_id = 94212853, start_date = as_date("2020-01-01"), end_date = as_date("2022−09−22"), probl = 2),
  tibble(site = "X06", tomst_id = 94217004, start_date = as_date("2020-01-01"), end_date = as_date("2022−09−22"), probl = 2),
  tibble(site = "X07", tomst_id = 94212872, start_date = as_date("2020-01-01"), end_date = as_date("2022−09−22"), probl = 2),
  tibble(site = "X08", tomst_id = 94213526, start_date = as_date("2020-01-01"), end_date = as_date("2022−09−22"), probl = 2),
  tibble(site = "X08", tomst_id = 94213526, start_date = as_date("2023-05-20"), end_date = as_date("2023−07−18"), probl = 1),
  tibble(site = "X08", tomst_id = 94213526, start_date = as_date("2023-08-09"), end_date = as_date("2023−09−03"), probl = 1),
  tibble(site = "X09", tomst_id = 94217042, start_date = as_date("2020-01-01"), end_date = as_date("2022−09−22"), probl = 2),
  tibble(site = "X09", tomst_id = 94217042, start_date = as_date("2023-08-18"), end_date = as_date("2023−09−03"), probl = 1),
  tibble(site = "X10", tomst_id = 94212883, start_date = as_date("2020-01-01"), end_date = as_date("2022−09−22"), probl = 2),
  tibble(site = "X11", tomst_id = 94212884, start_date = as_date("2020-01-01"), end_date = as_date("2022−09−22"), probl = 2),
  tibble(site = "X12", tomst_id = 94213529, start_date = as_date("2020-01-01"), end_date = as_date("2022−09−22"), probl = 2),
  tibble(site = "X13", tomst_id = 94217009, start_date = as_date("2020-01-01"), end_date = as_date("2022−09−22"), probl = 2),
  tibble(site = "X14", tomst_id = 94212854, start_date = as_date("2020-01-01"), end_date = as_date("2022−09−22"), probl = 2),
  tibble(site = "X15", tomst_id = 94217053, start_date = as_date("2020-01-01"), end_date = as_date("2022−09−23"), probl = 2),
  tibble(site = "X16", tomst_id = 94217027, start_date = as_date("2020-01-01"), end_date = as_date("2022−09−23"), probl = 2),
  tibble(site = "X17", tomst_id = 94213537, start_date = as_date("2020-01-01"), end_date = as_date("2022−09−23"), probl = 2),
  tibble(site = "X18", tomst_id = 94217059, start_date = as_date("2020-01-01"), end_date = as_date("2022−09−23"), probl = 2),
  tibble(site = "X19", tomst_id = 94213505, start_date = as_date("2020-01-01"), end_date = as_date("2022−09−23"), probl = 2),
  tibble(site = "X20", tomst_id = 94217080, start_date = as_date("2020-01-01"), end_date = as_date("2022−09−23"), probl = 2),
  tibble(site = "X21", tomst_id = 94217030, start_date = as_date("2020-01-01"), end_date = as_date("2022−09−23"), probl = 2),
  tibble(site = "X22", tomst_id = 94212867, start_date = as_date("2020-01-01"), end_date = as_date("2022−09−23"), probl = 2),
  tibble(site = "X23", tomst_id = 94213501, start_date = as_date("2020-01-01"), end_date = as_date("2022−09−23"), probl = 2),
  tibble(site = "X25", tomst_id = 94212866, start_date = as_date("2020-01-01"), end_date = as_date("2022−09−25"), probl = 2),
  tibble(site = "X26", tomst_id = 94217029, start_date = as_date("2020-01-01"), end_date = as_date("2022−09−25"), probl = 2),
  tibble(site = "X27", tomst_id = 94217074, start_date = as_date("2020-01-01"), end_date = as_date("2022−09−25"), probl = 2),
  tibble(site = "X28", tomst_id = 94212879, start_date = as_date("2020-01-01"), end_date = as_date("2022−09−25"), probl = 2),
  tibble(site = "X29", tomst_id = 94217076, start_date = as_date("2020-01-01"), end_date = as_date("2022−09−25"), probl = 2),
  tibble(site = "X30", tomst_id = 94212878, start_date = as_date("2020-01-01"), end_date = as_date("2022−09−25"), probl = 2),
  tibble(site = "X36", tomst_id = 94217054, start_date = as_date("2020-01-01"), end_date = as_date("2022−09−27"), probl = 2),
  tibble(site = "X38", tomst_id = 94217065, start_date = as_date("2020-01-01"), end_date = as_date("2022−09−27"), probl = 2),
  tibble(site = "X39", tomst_id = 94217073, start_date = as_date("2020-01-01"), end_date = as_date("2022−09−27"), probl = 2),
  tibble(site = "X40", tomst_id = 94217044, start_date = as_date("2020-01-01"), end_date = as_date("2022−09−27"), probl = 2)
)
el %>% filter(site == "X28")
el %>% filter(tomst_id == "94212879")
visittimes %>% filter(site == "L12")

nel %>% filter(start_date > end_date)

el <- bind_rows(el,
                nel) %>% 
  unique()

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

# Delete all data from L26 & L27 prior 2022−07−24

df <- df %>% 
  filter(!(site == "L26" & datetime < as_date("2022−07−24"))) %>% 
  filter(!(site == "L27" & datetime < as_date("2022−07−24")))

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
# 36 Tomsts not yet done

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

###############################################################################
# Combine with data from previous years

# Mark temporarily this as new data
df <- df %>% 
  mutate(datanew = TRUE) %>% 
  select(-zone,-mins) %>% 
  rename(error_tomst = probl)

# Combine with previous years Tomst data
od <- fread(paste0(old_data_dir,"/tomst_data_raw.csv")) %>%
  mutate(datetime = with_tz(datetime, tzone = "Etc/GMT-2")) %>% 
  mutate(datanew = FALSE) %>% 
  filter(!(is.na(T1) | is.na(T2) | is.na(T3) | is.na(moist)))

od <- od %>% 
  mutate(site = gsub("SAA","MI",site)) %>% 
  mutate(site = ifelse(site == "MI019", "MI19", site))

sites[!sites %in% unique(od$site)] # These are fine

df <- bind_rows(od,df) %>% 
  arrange(site, datetime)

df <- df %>% 
  distinct(site, datetime, .keep_all = T)

########################################################################
# FILL MISSING TIMESTAMPS WITH NA

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
    scale_y_continuous(limits = c(-20, 35))+
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
    scale_y_continuous(limits = c(400, 4000))+
    ggtitle(i) -> GG2
  
  print(plot_grid(plotlist = list(GG1, GG2), nrow = 2))
  
}
dev.off()

fwrite(df %>% select(-datanew), paste0("output/",area,"/tomst_data_raw.csv"))

####################################################################################