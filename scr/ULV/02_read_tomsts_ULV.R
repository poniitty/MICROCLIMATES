library(tidyverse)
library(lubridate)
library(data.table)
library(cowplot)
library(zoo)

area <- "ULV"

invisible(lapply(list.files("scr/functions/", ".R$", full.names = T), source))

# Set date limits to remove implausible dates
mind <- as.Date("2018-06-01", tz = "Etc/GMT-2")
maxd <- as.Date("2023-11-01", tz = "Etc/GMT-2")

raw_data_dir <- "/scratch/project_2007415/DATA2023/Ulvinsalo2023"
# old_data_dir <- "/projappl/project_2003061/repos/microclim_evo/output/"
temp_dir <- "/scratch/project_2007415/temp/"

#################################################################################3
# Read earlier visiting/reading times

# visittimes <- read_csv(paste0("data/",area,"/reading_times_",area,".csv")) %>%
#   mutate(maxdt = with_tz(maxdt, tzone = "Etc/GMT-2"))
visittimes <- read_csv(paste0("data/",area,"/initial_site_codes.csv")) %>% mutate(maxdt = NA)

# List logger data files to read
f <- list.files(raw_data_dir, pattern = "^data_", full.names = T, recursive = T)

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
  distinct() %>% 
  drop_na()

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
monthly_tomst_plots(siteids = sites[60:114], df = df, visdir = temp_dir, months_to_plot = times)

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
  tibble(site = "ULV001", tomst_id = 94214276, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−31"), probl = 2),
  tibble(site = "ULV002", tomst_id = 94214162, start_date = as_date("2021−05−01"), end_date = as_date("2022-05-30"), probl = 2),
  tibble(site = "ULV003", tomst_id = 94214295, start_date = as_date("2021−05−01"), end_date = as_date("2022-05-31"), probl = 2),
  tibble(site = "ULV003", tomst_id = 94214295, start_date = as_date("2022-10-03"), end_date = as_date("2023-08-09"), probl = 1),
  tibble(site = "ULV004", tomst_id = 94214198, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−25"), probl = 2),
  tibble(site = "ULV005", tomst_id = 94214189, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−26"), probl = 2),
  tibble(site = "ULV007", tomst_id = 94214221, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−26"), probl = 2),
  tibble(site = "ULV008", tomst_id = 94214359, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−22"), probl = 2),
  tibble(site = "ULV009", tomst_id = 94214213, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−25"), probl = 2),
  tibble(site = "ULV010", tomst_id = 94214353, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−22"), probl = 2),
  tibble(site = "ULV011", tomst_id = 94214209, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−22"), probl = 2),
  tibble(site = "ULV012", tomst_id = 94214197, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−25"), probl = 2),
  tibble(site = "ULV012", tomst_id = 94214197, start_date = as_date("2022-05-26"), end_date = as_date("2023-08-09"), probl = 3),
  tibble(site = "ULV013", tomst_id = 94214184, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−23"), probl = 2),
  tibble(site = "ULV014", tomst_id = 94214187, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−27"), probl = 2),
  tibble(site = "ULV015", tomst_id = 94214223, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−31"), probl = 2),
  tibble(site = "ULV016", tomst_id = 94214220, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−26"), probl = 2),
  tibble(site = "ULV017", tomst_id = 94214242, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−23"), probl = 2),
  tibble(site = "ULV017", tomst_id = 94214242, start_date = as_date("2022-05-24"), end_date = as_date("2023-08-09"), probl = 3),
  tibble(site = "ULV018", tomst_id = 94214156, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−30"), probl = 2),
  tibble(site = "ULV019", tomst_id = 94214355, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−21"), probl = 2),
  tibble(site = "ULV020", tomst_id = 94214350, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−21"), probl = 2),
  tibble(site = "ULV021", tomst_id = 94214218, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−26"), probl = 2),
  tibble(site = "ULV022", tomst_id = 94214207, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−22"), probl = 2),
  tibble(site = "ULV023", tomst_id = 94214214, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−26"), probl = 2),
  tibble(site = "ULV024", tomst_id = 94214231, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−27"), probl = 2),
  tibble(site = "ULV024", tomst_id = 94214231, start_date = as_date("2022−05−28"), end_date = as_date("2023-08-07"), probl = 3),
  tibble(site = "ULV025", tomst_id = 94214319, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−21"), probl = 2),
  tibble(site = "ULV026", tomst_id = 94214323, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−31"), probl = 2),
  tibble(site = "ULV026", tomst_id = 94214323, start_date = as_date("2023-05-22"), end_date = as_date("2023-08-09"), probl = 1),
  tibble(site = "ULV027", tomst_id = 94214243, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−23"), probl = 2),
  tibble(site = "ULV028", tomst_id = 94214181, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−23"), probl = 2),
  tibble(site = "ULV029", tomst_id = 94214316, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−21"), probl = 2),
  tibble(site = "ULV030", tomst_id = 94214248, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−25"), probl = 2),
  tibble(site = "ULV031", tomst_id = 94214200, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−25"), probl = 2),
  tibble(site = "ULV031", tomst_id = 94214200, start_date = as_date("2022-10-15"), end_date = as_date("2023-08-10"), probl = 1),
  tibble(site = "ULV032", tomst_id = 94214160, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−31"), probl = 2),
  tibble(site = "ULV033", tomst_id = 94214205, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−22"), probl = 2),
  tibble(site = "ULV034", tomst_id = 94214211, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−25"), probl = 2),
  tibble(site = "ULV035", tomst_id = 94214224, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−26"), probl = 2),
  tibble(site = "ULV036", tomst_id = 94214183, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−23"), probl = 2),
  tibble(site = "ULV037", tomst_id = 94214229, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−26"), probl = 2),
  tibble(site = "ULV038", tomst_id = 94214228, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−26"), probl = 2),
  tibble(site = "ULV039", tomst_id = 94214188, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−27"), probl = 2),
  tibble(site = "ULV040", tomst_id = 94214163, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−30"), probl = 2),
  tibble(site = "ULV041", tomst_id = 94214278, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−27"), probl = 2),
  tibble(site = "ULV042", tomst_id = 94214260, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−31"), probl = 2),
  tibble(site = "ULV043", tomst_id = 94214241, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−25"), probl = 2),
  tibble(site = "ULV044", tomst_id = 94214249, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−25"), probl = 2),
  tibble(site = "ULV045", tomst_id = 94214202, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−23"), probl = 2),
  tibble(site = "ULV046", tomst_id = 94214280, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−27"), probl = 2),
  tibble(site = "ULV047", tomst_id = 94214206, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−22"), probl = 2),
  tibble(site = "ULV048", tomst_id = 94214219, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−26"), probl = 2),
  tibble(site = "ULV049", tomst_id = 94214204, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−23"), probl = 2),
  tibble(site = "ULV050", tomst_id = 94214245, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−23"), probl = 2),
  tibble(site = "ULV051", tomst_id = 94214157, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−30"), probl = 2),
  tibble(site = "ULV052", tomst_id = 94214317, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−21"), probl = 2),
  tibble(site = "ULV052", tomst_id = 94214317, start_date = as_date("2022-08-11"), end_date = as_date("2023-08-08"), probl = 1),
  tibble(site = "ULV053", tomst_id = 94214216, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−26"), probl = 2),
  tibble(site = "ULV054", tomst_id = 94214217, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−26"), probl = 2),
  tibble(site = "ULV055", tomst_id = 94214250, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−25"), probl = 2),
  tibble(site = "ULV056", tomst_id = 94214215, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−25"), probl = 2),
  tibble(site = "ULV057", tomst_id = 94214318, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−22"), probl = 2),
  tibble(site = "ULV058", tomst_id = 94214354, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−21"), probl = 2),
  tibble(site = "ULV059", tomst_id = 94214294, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−31"), probl = 2),
  tibble(site = "ULV059", tomst_id = 94214294, start_date = as_date("2022-09-04"), end_date = as_date("2023-08-08"), probl = 1),
  tibble(site = "ULV060", tomst_id = 94214320, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−21"), probl = 2),
  tibble(site = "ULV061", tomst_id = 94214246, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−25"), probl = 2),
  tibble(site = "ULV061", tomst_id = 94214246, start_date = as_date("2022-05-29"), end_date = as_date("2023-08-10"), probl = 1),
  tibble(site = "ULV062", tomst_id = 94214347, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−22"), probl = 2),
  tibble(site = "ULV063", tomst_id = 94214349, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−22"), probl = 2),
  tibble(site = "ULV064", tomst_id = 94214357, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−21"), probl = 2),
  tibble(site = "ULV065", tomst_id = 94214348, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−22"), probl = 2),
  tibble(site = "ULV067", tomst_id = 94214182, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−23"), probl = 2),
  tibble(site = "ULV068", tomst_id = 94214164, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−30"), probl = 2),
  tibble(site = "ULV068", tomst_id = 94214164, start_date = as_date("2022-06-16"), end_date = as_date("2023-08-07"), probl = 1),
  tibble(site = "ULV069", tomst_id = 94214325, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−31"), probl = 2),
  tibble(site = "ULV070", tomst_id = 94214298, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−30"), probl = 2),
  tibble(site = "ULV071", tomst_id = 94214352, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−22"), probl = 2),
  tibble(site = "ULV072", tomst_id = 94214360, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−21"), probl = 2),
  tibble(site = "ULV073", tomst_id = 94214259, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−31"), probl = 2),
  tibble(site = "ULV074", tomst_id = 94214212, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−25"), probl = 2),
  tibble(site = "ULV075", tomst_id = 94214226, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−26"), probl = 2),
  tibble(site = "ULV075", tomst_id = 94214226, start_date = as_date("2022−05−27"), end_date = as_date("2023-08-09"), probl = 3),
  tibble(site = "ULV076", tomst_id = 94214210, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−22"), probl = 2),
  tibble(site = "ULV077", tomst_id = 94214196, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−25"), probl = 2),
  tibble(site = "ULV077", tomst_id = 94214196, start_date = as_date("2022-06-18"), end_date = as_date("2023-08-10"), probl = 1),
  tibble(site = "ULV078", tomst_id = 94214165, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−30"), probl = 2),
  tibble(site = "ULV079", tomst_id = 94214279, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−27"), probl = 2),
  tibble(site = "ULV080", tomst_id = 94214161, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−30"), probl = 2),
  tibble(site = "ULV081", tomst_id = 94214293, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−31"), probl = 2),
  tibble(site = "ULV081", tomst_id = 94214293, start_date = as_date("2022−06−01"), end_date = as_date("2023-08-08"), probl = 3),
  tibble(site = "ULV082", tomst_id = 94214234, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−27"), probl = 2),
  tibble(site = "ULV083", tomst_id = 94214185, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−23"), probl = 2),
  tibble(site = "ULV084", tomst_id = 94214208, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−22"), probl = 2),
  tibble(site = "ULV085", tomst_id = 94214203, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−23"), probl = 2),
  tibble(site = "ULV086", tomst_id = 94214321, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−31"), probl = 2),
  tibble(site = "ULV087", tomst_id = 94214292, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−31"), probl = 2),
  tibble(site = "ULV087", tomst_id = 94214292, start_date = as_date("2022-10-02"), end_date = as_date("2023-08-08"), probl = 1),
  tibble(site = "ULV088", tomst_id = 94214222, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−26"), probl = 2),
  tibble(site = "ULV089", tomst_id = 94214199, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−25"), probl = 2),
  tibble(site = "ULV090", tomst_id = 94214190, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−26"), probl = 2),
  tibble(site = "ULV091", tomst_id = 94214230, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−25"), probl = 2),
  tibble(site = "ULV091", tomst_id = 94214230, start_date = as_date("2022−05−26"), end_date = as_date("2023-08-09"), probl = 3),
  tibble(site = "ULV092", tomst_id = 94214247, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−26"), probl = 2),
  tibble(site = "ULV093", tomst_id = 94214158, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−30"), probl = 2),
  tibble(site = "ULV094", tomst_id = 94214186, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−27"), probl = 2),
  tibble(site = "ULV095", tomst_id = 94214346, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−22"), probl = 2),
  tibble(site = "ULV096", tomst_id = 94214351, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−22"), probl = 2),
  tibble(site = "ULV097", tomst_id = 94214358, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−21"), probl = 2),
  tibble(site = "ULV098", tomst_id = 94214356, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−21"), probl = 2),
  tibble(site = "ULV099", tomst_id = 94214288, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−28"), probl = 2),
  tibble(site = "ULV100", tomst_id = 94214300, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−30"), probl = 2),
  tibble(site = "ULV101", tomst_id = 94214291, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−31"), probl = 2),
  tibble(site = "ULV102", tomst_id = 94214258, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−31"), probl = 2),
  tibble(site = "ULV103", tomst_id = 94214322, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−31"), probl = 2),
  tibble(site = "ULV103", tomst_id = 94214322, start_date = as_date("2022−06−01"), end_date = as_date("2023-08-08"), probl = 3),
  tibble(site = "ULV104", tomst_id = 94214324, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−31"), probl = 2),
  tibble(site = "ULV105", tomst_id = 94214201, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−23"), probl = 2),
  tibble(site = "ULV106", tomst_id = 94214225, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−26"), probl = 2),
  tibble(site = "ULV108", tomst_id = 94214296, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−28"), probl = 2),
  tibble(site = "ULV109", tomst_id = 94214232, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−28"), probl = 2),
  tibble(site = "ULV109", tomst_id = 94214232, start_date = as_date("2022−05−29"), end_date = as_date("2023-08-08"), probl = 3),
  tibble(site = "ULV110", tomst_id = 94214299, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−28"), probl = 2),
  tibble(site = "ULV111", tomst_id = 94214227, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−28"), probl = 2),
  tibble(site = "ULV111", tomst_id = 94214227, start_date = as_date("2022−05−29"), end_date = as_date("2023-08-08"), probl = 3),
  tibble(site = "ULV112", tomst_id = 94214289, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−28"), probl = 2),
  tibble(site = "ULV113", tomst_id = 94214235, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−28"), probl = 2),
  tibble(site = "ULV114", tomst_id = 94214286, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−28"), probl = 2),
  tibble(site = "ULV115", tomst_id = 94214287, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−28"), probl = 2),
  tibble(site = "ULV116", tomst_id = 94214290, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−28"), probl = 2),
  tibble(site = "ULV117", tomst_id = 94214297, start_date = as_date("2021−05−01"), end_date = as_date("2022−05−28"), probl = 2),
  tibble(site = "ULV117", tomst_id = 94214297, start_date = as_date("2022-10-13"), end_date = as_date("2023-08-08"), probl = 1)
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