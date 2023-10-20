library(tidyverse)
library(lubridate)
library(sf)
library(zoo)

area <- "MAT"

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

df <- df %>% mutate(T1 = ifelse(rollmax((T1 < (-30) | T1 > 30), 3, align = "center", fill = 0) == 1, NA, T1),
                    T2 = ifelse(rollmax((T2 < (-60) | T2 > 50), 3, align = "center", fill = 0) == 1, NA, T2),
                    T3 = ifelse(rollmax((T3 < (-60) | T3 > 50), 3, align = "center", fill = 0) == 1, NA, T3),
                    moist = ifelse(rollmax((moist < 200 | moist >= 4080), 3, align = "center", fill = 0) == 1, NA, moist))

##############################################################################
# Filter by error code, i.e., remove values reported manually erroneous

df <- df %>%
  mutate(T1 = as.numeric(ifelse(error_tomst %in% c(1,2,4,9), NA, T1))) %>% 
  mutate(T2 = as.numeric(ifelse(error_tomst %in% c(1,2,7,8), NA, T2))) %>% 
  mutate(T3 = as.numeric(ifelse(error_tomst %in% c(1,2,4,5,7,8,10), NA, T3))) %>% 
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


# Read in site coordinates
p <- st_read("data/site_coordinates.gpkg") %>% 
  rename(area2 = area) %>% 
  filter(area2 == area) %>% 
  rename(Site_id = site,
         Latitude = Lat,
         Longitude = Lon) %>% 
  select(Site_id,Latitude,Longitude) %>% 
  st_drop_geometry()

# Get the first and last dates with data per sensor
times <- df %>% 
  pivot_longer(cols = T1:Soil_moisture, names_to = "Sensor_code") %>% 
  drop_na() %>% 
  group_by(Site_id, Raw_data_identifier, Sensor_code) %>% 
  summarise(fd = min(datetime) + days(1),
            ld = max(datetime)) %>% 
  mutate(Start_date_year = year(fd),
         Start_date_month = month(fd),
         Start_date_day = mday(fd)) %>% 
  mutate(End_date_year = year(ld),
         End_date_month = month(ld),
         End_date_day = mday(ld)) %>% 
  select(-fd,-ld)

# Get the measurement intervals per logger
frqs <- lapply(unique(df$Raw_data_identifier) %>% na.omit, function(i){
  
  df %>% filter(Raw_data_identifier == i) %>% 
    arrange(datetime) -> temp
  temp %>% mutate(timediff = as.numeric(datetime - lag(datetime))) -> temp
  
  x <- tibble(Raw_data_identifier = i,
              Temporal_resolution = table(na.omit(temp$timediff)) %>% sort %>% head(1) %>% names %>% as.numeric)
  
  return(x)
}) %>% 
  bind_rows()

# Combine
meta <- right_join(p, times %>% mutate(Grid = unlist(lapply(Site_id, function(x) substr(x, 1, 5))))) %>% 
  left_join(., frqs) %>% 
  select(-Grid)


# Add rest of the metadata
meta <- meta %>% 
  mutate(meta_id = row_number()) %>% 
  mutate(Country_code = "NO",
         Experiment_name = "Mattavarri",
         Experimental_manipulation = "No",
         Experiment_insitu = "Yes",
         Experiment_climate = "No",
         Experiment_citizens = "No",
         Experiment_design = "",
         Experiment_doi = "",
         Experiment_comment = "",
         Habitat_type = 4,
         Habitat_sub_type = 1,
         Site_comments = "",
         Logger_code = Raw_data_identifier,
         EPSG = 4326,
         GPS_accuracy = 5,
         Elevation = NA,
         Logger_serial_number = Raw_data_identifier,
         Logger_brand = "TOMST",
         Logger_type = "TMS4",
         Logger_age = 2021,
         Logger_comment = "",
         Timezone = "UTC",
         Time_difference = 0,
         Licence = "CC-BY",
         Sensor_comments = "",
         Species_composition = "Yes",
         Species_trait = "Yes")


# Add sensor specific metadata
bind_rows(meta %>% filter(Sensor_code == "T1") %>% 
            mutate(Sensor_shielding = "No",
                   Sensor_shielding_type = NA,
                   Microclimate_measurement = "Temperature",
                   Unit = "°C",
                   Sensor_accuracy = 0.5,
                   Sensor_height = -6,
                   Sensor_length = NA),
          meta %>% filter(Sensor_code == "T2") %>% 
            mutate(Sensor_shielding = "Yes",
                   Sensor_shielding_type = "No",
                   Microclimate_measurement = "Temperature",
                   Unit = "°C",
                   Sensor_accuracy = 0.5,
                   Sensor_height = 2,
                   Sensor_length = NA),
          meta %>% filter(Sensor_code == "T3") %>% 
            mutate(Sensor_shielding = "Yes",
                   Sensor_shielding_type = "No",
                   Microclimate_measurement = "Temperature",
                   Unit = "°C",
                   Sensor_accuracy = 0.5,
                   Sensor_height = 15,
                   Sensor_length = NA),
          meta %>% filter(Sensor_code == "Soil_moisture") %>% 
            mutate(Sensor_shielding = "No",
                   Sensor_shielding_type = NA,
                   Microclimate_measurement = "Soil_moisture",
                   Unit = "raw",
                   Sensor_accuracy = 1,
                   Sensor_height = -7.5,
                   Sensor_length = 15)) %>% 
  arrange(meta_id) -> meta

# Read in the column names for SoilTemp metadata
cns <- read_csv("data/SoilTemp_colnames.csv")

# Check if all needed is there
names(meta)[!names(meta) %in% cns$Col_name]
cns$Col_name[!cns$Col_name %in% names(meta)]
# All good if none

# Reorder the columns
meta <- meta %>% 
  select(cns$Col_name)

# Finalize the time columns in time series data
df %>% mutate(Year = year(datetime),
              Month = month(datetime),
              Day = mday(datetime),
              Time = format(datetime, "%H:%M", tz = "GMT")) %>% 
  select(Raw_data_identifier, Year, Month, Day, Time, T1, T2, T3, Soil_moisture) -> df

summary(df)
summary(meta)
nrow(meta)

# Write out the final files. Don't forget the fill out the PEOPLE.csv file as well!!
write_csv(meta, paste0("output/",area,"/METADATA.csv"))
write_csv(df, paste0("output/",area,"/RAW-TIME-SERIES-DATA.csv"))
