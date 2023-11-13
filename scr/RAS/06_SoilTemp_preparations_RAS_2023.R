library(tidyverse)
library(lubridate)
library(sf)
library(zoo)

area <- "RAS"

invisible(lapply(list.files("scr/functions/", ".R$", full.names = T), source))

d <- read_csv(paste0("output/",area,"/tomst_data_raw.csv"))

library(tidyverse)
library(lubridate)
library(sf)
library(zoo)

area <- "SA"

invisible(lapply(list.files("scr/functions/", ".R$", full.names = T), source))

df <- read_csv(paste0("output/",area,"/tomst_data_raw.csv"))

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

df <- df %>% mutate(T1 = ifelse(rollmax((T1 < (-20) | T1 > 30), 3, align = "center", fill = 0) == 1, NA, T1),
                    T2 = ifelse(rollmax((T2 < (-60) | T2 > 50), 3, align = "center", fill = 0) == 1, NA, T2),
                    T3 = ifelse(rollmax((T3 < (-60) | T3 > 50), 3, align = "center", fill = 0) == 1, NA, T3),
                    moist = ifelse(rollmax((moist < 200 | moist >= 4096), 3, align = "center", fill = 0) == 1, NA, moist))

##############################################################################
# Filter by error code, i.e., remove values reported manually erroneous

df <- df %>%
  mutate(T1 = as.numeric(ifelse(error_tomst %in% c(1,2,4,9), NA, T1))) %>% 
  mutate(T2 = as.numeric(ifelse(error_tomst %in% c(1,2,7,8), NA, T2))) %>% 
  mutate(T3 = as.numeric(ifelse(error_tomst %in% c(1,2,3,4,5,7,8,10), NA, T3))) %>% 
  mutate(moist = as.numeric(ifelse(error_tomst %in% c(1,2,6,8,9,10), NA, moist)))
gc()

###############################################################################
# Filter out no-measurement moments
df <- df %>% filter(if_any(T1:moist, ~ !is.na(.)))

###############################################################################
# FILL MISSING TIMESTAMPS WITH NA

tids <- unique(df$site)

df <- lapply(tids, fill_timestamps_site, df = df) %>% 
  bind_rows()

df <- df %>% 
  distinct(site, datetime, .keep_all = T) %>% 
  ungroup()

# Renaming for SoilTemp format
df %>% 
  rename(Site_id = site,
         Raw_data_identifier = tomst_id,
         Soil_moisture = moist) %>% 
  select(Site_id, Raw_data_identifier, datetime, T1, T2, T3, Soil_moisture) -> df

############################################
# META

p <- st_read("data/site_coordinates.gpkg") %>% 
  filter(area == area)

p %>% mutate(Location_code = site,
             Latitude = Lat,
             Longitude = Lon) %>% 
  st_drop_geometry() %>% 
  select(Location_code:Longitude) %>% 
  mutate(Raw_data_identifier = paste0(Location_code, "_tomst"),
         EPSG = 4326,
         GPS_accuracy = 5) -> p


d %>% select(Raw_data_identifier, T1:Soil_moisture) %>% 
  mutate(Country_code = "NO") %>% 
  pivot_longer(cols = T1:Soil_moisture, names_to = "Sensor_code") %>% 
  group_by(Country_code, Raw_data_identifier, Sensor_code) %>% 
  count() %>% 
  select(-n) -> meta

meta %>% mutate(Location_code = gsub("_tomst", "", Raw_data_identifier),
                Logger_code = Raw_data_identifier) %>% 
  relocate(Location_code, .after = Country_code) %>% 
  relocate(Logger_code, .after = Location_code) %>% 
  mutate(Sensor_height = ifelse(Sensor_code == "Soil_moisture", -15, NA),
         Sensor_height = ifelse(Sensor_code == "T1", -6, Sensor_height),
         Sensor_height = ifelse(Sensor_code == "T2", 2, Sensor_height),
         Sensor_height = ifelse(Sensor_code == "T3", 15, Sensor_height)) %>% 
  relocate(Raw_data_identifier, .after = Sensor_height) %>% 
  mutate(Microclimate_measurement = "Temperature",
         Microclimate_measurement = ifelse(Sensor_code == "Soil_moisture", "Soil_moisture", Microclimate_measurement)) -> meta


left_join(meta, p) %>% 
  mutate(Logger_brand = "Tomst",
         Logger_type = "TMS-4",
         Sensor_accuracy = 0.5,
         Logger_shielding = "Shield") %>% 
  mutate(Logger_shielding = ifelse(Sensor_code %in% c("T1","Soil_moisture"), "No shield", Logger_shielding)) -> meta


d %>% mutate(date = as_date(paste(Year,Month,Day,sep = "_"))) %>% 
  select(Raw_data_identifier, date, T1:Soil_moisture) %>% 
  pivot_longer(cols = T1:Soil_moisture, names_to = "Sensor_code") %>% 
  filter(!is.na(value)) %>% 
  group_by(Raw_data_identifier, Sensor_code) %>% 
  summarise(mind = min(date),
            maxd = max(date)) %>% 
  mutate(Start_date_year = year(mind),
         Start_date_month = month(mind),
         Start_date_date = day(mind),
         End_date_year = year(maxd),
         End_date_month = month(maxd),
         End_date_date = day(maxd)) %>% 
  select(-mind,-maxd) -> dates

left_join(meta, dates) %>% 
  mutate(Temporal_resolution = 15,
         Timezone = "UTC") %>% 
  mutate(Species_composition = "yes") %>% 
  mutate(Species_trait = "no") -> meta

meta %>% mutate(Plot_size = NA,
                Forest_canopy_cover = NA,
                Data_open_access = "yes",
                Meta_data_open_access = "yes",
                FirstName = "Tuuli",
                LastName = "Rissanen",
                Email = "tuuli.rissanen@helsinki.fi",
                Institute = "University of Helsinki",
                Other_contributors = "") -> meta

# HABITAT CLASSES

meta %>% 
  mutate(Habitat_type = "4",
         Habitat_sub_type = "4.2") -> meta

meta %>% relocate(Habitat_type:Habitat_sub_type, .after = Logger_shielding) -> meta

write_csv(meta, "output/RAS/SoilTemp_NO_TR_RASTI_meta.csv")
write_csv(d, "output/RAS/SoilTemp_NO_TR_RASTI.csv")
