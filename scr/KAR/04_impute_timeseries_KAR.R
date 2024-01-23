library(tidyverse)
library(missRanger, lib.loc = "/projappl/project_2003061/Rpackages")
library(lubridate)
library(data.table)

area <- "KAR"

df <- fread(paste0("output/",area,"/tomst_data_cleaned.csv")) %>% 
  mutate(datetime = with_tz(datetime, tzone = "Etc/GMT-2")) %>% 
  filter(!is.na(datetime))

unique(df$site)


# T1

dfw <- df %>% 
  select(site, datetime, T1) %>% 
  pivot_wider(id_cols = datetime, names_from = site, values_from = T1) %>% 
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

df_T1 <- df %>% 
  select(site, datetime, T1) %>% 
  pivot_wider(id_cols = datetime, names_from = site, values_from = T1) %>% 
  arrange(datetime) %>% 
  mutate(id = 1:nrow(.)) %>% 
  select(datetime, id) %>% 
  left_join(.,
            dfw %>% 
              select(-yd,-hour) %>% 
              pivot_longer(cols = c(-id), names_to = "site", values_to = "T1") %>% 
              mutate(T1_imp = ifelse(is.na(T1), TRUE, FALSE)) %>% 
              select(-T1)) %>% 
  left_join(.,
            dfwimp %>% 
              select(-yd,-hour) %>% 
              pivot_longer(cols = c(-id), names_to = "site", values_to = "T1")) %>% 
  arrange(site, datetime) %>% 
  select(-id)

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
                     pmm.k = 3, num.trees = 50, maxiter = 10,
                     num.threads = future::availableCores())


df_T2 <- df %>% 
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

# T3

dfw <- df %>% 
  select(site, datetime, T3) %>% 
  pivot_wider(id_cols = datetime, names_from = site, values_from = T3) %>% 
  arrange(datetime) %>% 
  mutate(id = 1:nrow(.),
         yd = yday(datetime),
         hour = hour(datetime)) %>% 
  relocate(id, yd, hour) %>% 
  select(-datetime)

# Imputation, this may take tens of minutes...
dfwimp <- missRanger(dfw,
                     pmm.k = 3, num.trees = 50, maxiter = 10,
                     num.threads = future::availableCores())


df_T3 <- df %>% 
  select(site, datetime, T3) %>% 
  pivot_wider(id_cols = datetime, names_from = site, values_from = T3) %>% 
  arrange(datetime) %>% 
  mutate(id = 1:nrow(.)) %>% 
  select(datetime, id) %>% 
  left_join(.,
            dfw %>% 
              select(-yd,-hour) %>% 
              pivot_longer(cols = c(-id), names_to = "site", values_to = "T3") %>% 
              mutate(T3_imp = ifelse(is.na(T3), TRUE, FALSE)) %>% 
              select(-T3)) %>% 
  left_join(.,
            dfwimp %>% 
              select(-yd,-hour) %>% 
              pivot_longer(cols = c(-id), names_to = "site", values_to = "T3")) %>% 
  arrange(site, datetime) %>% 
  select(-id)

# moist

dfw <- df %>% 
  mutate(moist = ifelse(T1 < 1 | is.na(T1), NA, moist)) %>% 
  select(site, datetime, moist) %>% 
  pivot_wider(id_cols = datetime, names_from = site, values_from = moist) %>% 
  arrange(datetime) %>% 
  mutate(id = 1:nrow(.),
         yd = yday(datetime),
         hour = hour(datetime)) %>% 
  relocate(id, yd, hour) %>% 
  select(-datetime)


dfw <- dfw %>% 
  rowwise() %>%
  mutate(NA_prop = mean(is.na(across(starts_with(area))))) %>% 
  filter(NA_prop < 0.8) %>% 
  select(-NA_prop) %>% 
  ungroup()

# Imputation, this may take tens of minutes...
dfwimp <- missRanger(dfw,
                     pmm.k = 3, num.trees = 50, maxiter = 10,
                     num.threads = future::availableCores())


df_moist <- df %>% 
  select(site, datetime, moist) %>% 
  pivot_wider(id_cols = datetime, names_from = site, values_from = moist) %>% 
  arrange(datetime) %>% 
  mutate(id = 1:nrow(.)) %>% 
  select(datetime, id) %>% 
  left_join(.,
            dfw %>% 
              select(-yd,-hour) %>% 
              pivot_longer(cols = c(-id), names_to = "site", values_to = "moist") %>% 
              mutate(moist_imp = ifelse(is.na(moist), TRUE, FALSE)) %>% 
              select(-moist)) %>% 
  left_join(.,
            dfwimp %>% 
              select(-yd,-hour) %>% 
              pivot_longer(cols = c(-id), names_to = "site", values_to = "moist")) %>% 
  arrange(site, datetime) %>% 
  select(-id)

df_moist <- df %>% 
  select(site, datetime, moist) %>% 
  rename(moist_orig = moist) %>% 
  full_join(., df_moist) %>% 
  mutate(moist_imp = ifelse(is.na(moist), FALSE, moist_imp),
         moist = ifelse(is.na(moist), moist_orig, moist)) %>% 
  full_join(., df_T1) %>% 
  mutate(moist = ifelse(T1 < 1, NA, moist),
         moist_imp = ifelse(is.na(moist), NA, moist_imp)) %>% 
  select(site, datetime, moist_imp, moist)

################################################################
# Combine

df_all <- full_join(df_T1,
          df_T2) %>% 
  full_join(.,
            df_T3) %>% 
  full_join(.,
            df_moist) %>% 
  relocate(ends_with("_imp"), .after = moist)

df_all %>% write_csv(paste0("output/",area,"/tomst_data_imputed.csv"))

