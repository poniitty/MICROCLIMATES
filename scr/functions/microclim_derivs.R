
microclim_derivs <- function(df_micro, quant){
  library(tidyverse)
  library(lubridate)
  library(scales)
  # Create more time variables and add empty days for incomplete years
  df_micro <- df_micro %>% 
    mutate(date = as_date(datetime)) %>% 
    mutate(year = year(date))
  
  df_micro <- df_micro %>% 
    full_join(.,
              tibble(date = seq.Date(as_date(paste0(min(df_micro$year),"-01-01")), as_date(paste0(max(df_micro$year),"-12-31")), by = 1)),
              by = join_by(date)) %>% 
    mutate(hyddate = date + days(122)) %>% 
    mutate(year = year(date),
           hydyear = year(hyddate),
           month = month(date),
           doy = yday(date),
           hyddoy = yday(hyddate)) %>% 
    relocate(hyddate:hyddoy, .after = date)
  
  sites <- unique(df_micro$site) %>% na.omit %>% sort
  
  dall <- lapply(sites, microclim_derivs_inner, df_micro = df_micro, quant = quant)
  
  dall <- dall %>% 
    bind_rows()
  
  return(dall)
}

microclim_derivs_inner <- function(siteid, df_micro, quant){
  
  print(siteid)
  temp <- df_micro %>% filter(site == siteid | is.na(site)) %>% 
    arrange(date, datetime)
  
  tempd <- temp %>% 
    group_by(date) %>% 
    summarise(across(c(year,hydyear,month,doy,hyddoy,T1:T3,T1_imp:T3_imp), ~mean(.x, na.rm = F)))
  
  # Annual means
  d1 <- temp %>% 
    group_by(year) %>% 
    summarise(across(c(T1:T3), ~mean(.x, na.rm = F)),
              round2(across(T1_imp:T3_imp, ~mean(.x, na.rm = F)*100))) %>% 
    drop_na() %>% 
    mutate(period = "annual", 
           var = "mean")
  
  # TDD
  d2 <- tempd %>% 
    group_by(year) %>% 
    mutate(T1_imp = ifelse(T1 < 0, NA, T1_imp),
           T2_imp = ifelse(T2 < 0, NA, T2_imp),
           T3_imp = ifelse(T3 < 0, NA, T3_imp)) %>% 
    summarise(across(T1:T3, ~therm_sum(.x, thrh = 0, direc = "above", na.rm = F)),
              round2(across(T1_imp:T3_imp, ~mean(.x, na.rm = T)*100))) %>% 
    drop_na() %>% 
    mutate(period = "annual", 
           var = "TDD")
  
  d3 <- tempd %>% 
    group_by(year) %>% 
    mutate(T1_imp = ifelse(T1 < 0, NA, T1_imp),
           T2_imp = ifelse(T2 < 0, NA, T2_imp),
           T3_imp = ifelse(T3 < 0, NA, T3_imp)) %>% 
    summarise(across(T1:T3, ~sum(.x > 0, na.rm = F)),
              round2(across(T1_imp:T3_imp, ~mean(.x, na.rm = T)*100))) %>% 
    drop_na() %>% 
    mutate(period = "annual", 
           var = "TDD_days")
  
  # GDD3
  d4 <- tempd %>% 
    group_by(year) %>% 
    mutate(T1_imp = ifelse(T1 < 3, NA, T1_imp),
           T2_imp = ifelse(T2 < 3, NA, T2_imp),
           T3_imp = ifelse(T3 < 3, NA, T3_imp)) %>% 
    summarise(across(T1:T3, ~therm_sum(.x, thrh = 3, direc = "above", na.rm = F)),
              round2(across(T1_imp:T3_imp, ~mean(.x, na.rm = T)*100))) %>% 
    drop_na() %>% 
    mutate(period = "annual", 
           var = "GDD3")
  
  d5 <- tempd %>% 
    group_by(year) %>% 
    mutate(T1_imp = ifelse(T1 < 3, NA, T1_imp),
           T2_imp = ifelse(T2 < 3, NA, T2_imp),
           T3_imp = ifelse(T3 < 3, NA, T3_imp)) %>% 
    summarise(across(T1:T3, ~sum(.x >= 3, na.rm = F)),
              round2(across(T1_imp:T3_imp, ~mean(.x, na.rm = T)*100))) %>% 
    drop_na() %>% 
    mutate(period = "annual", 
           var = "GDD3_days")
  
  # GDD5
  d6 <- tempd %>% 
    group_by(year) %>% 
    mutate(T1_imp = ifelse(T1 < 5, NA, T1_imp),
           T2_imp = ifelse(T2 < 5, NA, T2_imp),
           T3_imp = ifelse(T3 < 5, NA, T3_imp)) %>% 
    summarise(across(T1:T3, ~therm_sum(.x, thrh = 5, direc = "above", na.rm = F)),
              round2(across(T1_imp:T3_imp, ~mean(.x, na.rm = T)*100))) %>% 
    drop_na() %>% 
    mutate(period = "annual", 
           var = "GDD5")
  
  d7 <- tempd %>% 
    group_by(year) %>% 
    mutate(T1_imp = ifelse(T1 < 5, NA, T1_imp),
           T2_imp = ifelse(T2 < 5, NA, T2_imp),
           T3_imp = ifelse(T3 < 5, NA, T3_imp)) %>% 
    summarise(across(T1:T3, ~sum(.x >= 5, na.rm = F)),
              round2(across(T1_imp:T3_imp, ~mean(.x, na.rm = T)*100))) %>% 
    drop_na() %>% 
    mutate(period = "annual", 
           var = "GDD5_days")
  
  # FDD
  d8 <- tempd %>% 
    group_by(hydyear) %>% 
    mutate(T1_imp = ifelse(T1 > 0, NA, T1_imp),
           T2_imp = ifelse(T2 > 0, NA, T2_imp),
           T3_imp = ifelse(T3 > 0, NA, T3_imp)) %>% 
    summarise(across(T1:T3, ~therm_sum(.x, thrh = 0, direc = "below", na.rm = F)),
              round2(across(T1_imp:T3_imp, ~mean(.x, na.rm = T)*100))) %>% 
    drop_na() %>% 
    rename(year = hydyear) %>% 
    mutate(period = "annual", 
           var = "FDD")
  
  d9 <- tempd %>% 
    group_by(year) %>% 
    mutate(T1_imp = ifelse(T1 > 0, NA, T1_imp),
           T2_imp = ifelse(T2 > 0, NA, T2_imp),
           T3_imp = ifelse(T3 > 0, NA, T3_imp)) %>% 
    summarise(across(T1:T3, ~sum(.x < 0, na.rm = F)),
              round2(across(T1_imp:T3_imp, ~mean(.x, na.rm = T)*100))) %>% 
    drop_na() %>% 
    mutate(period = "annual", 
           var = "FDD_days")
  
  # Monthly variables
  d10 <- temp %>% 
    group_by(year, month) %>% 
    summarise(across(T1:T3, ~mean(.x, na.rm = F)),
              round2(across(T1_imp:T3_imp, ~mean(.x, na.rm = F)*100)),
              .groups = "drop") %>% 
    drop_na() %>% 
    mutate(period = month.abb[month], 
           var = "mean") %>% 
    select(-month)
  
  d10b <- temp %>% 
    group_by(date, year, month) %>% 
    summarise(across(T1:T3, ~diff(range(.x, na.rm = F))),
              round2(across(T1_imp:T3_imp, ~mean(.x, na.rm = F)*100)),
              .groups = "drop") %>% 
    group_by(year, month) %>% 
    summarise(across(T1:T3, ~mean(.x, na.rm = F)),
              round2(across(T1_imp:T3_imp, ~mean(.x, na.rm = F)*100)),
              .groups = "drop") %>% 
    drop_na() %>% 
    mutate(period = month.abb[month], 
           var = "mean_daily_range") %>% 
    ungroup() %>% 
    select(-month)
  
  miT1 <- temp %>% 
    group_by(year, month) %>% 
    mutate(n = round(n()*quant)) %>% 
    mutate(ss = sum(T1, na.rm = F)) %>% 
    filter(!is.na(ss)) %>% 
    mutate(n = ifelse(n < 1, 1, n)) %>% 
    arrange(year, month, T1) %>% 
    mutate(id = row_number()) %>% 
    filter(id == n) %>% 
    select(year, month, T1, T1_imp) %>% 
    drop_na() %>% 
    mutate(T1_imp = ifelse(T1_imp, 100, 0)) %>% 
    mutate(period = month.abb[month], 
           var = "min") %>% 
    ungroup() %>% 
    select(-month)
  
  miT2 <- temp %>% 
    group_by(year, month) %>% 
    mutate(n = round(n()*quant)) %>% 
    mutate(ss = sum(T1, na.rm = F)) %>% 
    filter(!is.na(ss)) %>% 
    mutate(n = ifelse(n < 1, 1, n)) %>% 
    arrange(year, month, T2) %>% 
    mutate(id = row_number()) %>% 
    filter(id == n) %>% 
    select(year, month, T2, T2_imp) %>% 
    drop_na() %>% 
    mutate(T2_imp = ifelse(T2_imp, 100, 0)) %>% 
    mutate(period = month.abb[month], 
           var = "min") %>% 
    ungroup() %>% 
    select(-month)
  
  miT3 <- temp %>% 
    group_by(year, month) %>% 
    mutate(n = round(n()*quant)) %>% 
    mutate(ss = sum(T1, na.rm = F)) %>% 
    filter(!is.na(ss)) %>% 
    mutate(n = ifelse(n < 1, 1, n)) %>% 
    arrange(year, month, T3) %>% 
    mutate(id = row_number()) %>% 
    filter(id == n) %>% 
    select(year, month, T3, T3_imp) %>% 
    drop_na() %>% 
    mutate(T3_imp = ifelse(T3_imp, 100, 0)) %>% 
    mutate(period = month.abb[month], 
           var = "min") %>% 
    ungroup() %>% 
    select(-month)
  
  # Monthly max
  
  maT1 <- temp %>% 
    group_by(year, month) %>% 
    mutate(n = round(n()*quant)) %>% 
    mutate(ss = sum(T1, na.rm = F)) %>% 
    filter(!is.na(ss)) %>% 
    mutate(n = ifelse(n < 1, 1, n)) %>% 
    arrange(year, month, desc(T1)) %>% 
    mutate(id = row_number()) %>% 
    filter(id == n) %>% 
    select(year, month, T1, T1_imp) %>% 
    drop_na() %>% 
    mutate(T1_imp = ifelse(T1_imp, 100, 0)) %>% 
    mutate(period = month.abb[month], 
           var = "max") %>% 
    ungroup() %>% 
    select(-month)
  
  maT2 <- temp %>% 
    group_by(year, month) %>% 
    mutate(n = round(n()*quant)) %>% 
    mutate(ss = sum(T2, na.rm = F)) %>% 
    filter(!is.na(ss)) %>% 
    mutate(n = ifelse(n < 1, 1, n)) %>% 
    arrange(year, month, desc(T2)) %>% 
    mutate(id = row_number()) %>% 
    filter(id == n) %>% 
    select(year, month, T2, T2_imp) %>% 
    drop_na() %>% 
    mutate(T2_imp = ifelse(T2_imp, 100, 0)) %>% 
    mutate(period = month.abb[month], 
           var = "max") %>% 
    ungroup() %>% 
    select(-month)
  
  maT3 <- temp %>% 
    group_by(year, month) %>% 
    mutate(n = round(n()*quant)) %>% 
    mutate(ss = sum(T3, na.rm = F)) %>% 
    filter(!is.na(ss)) %>% 
    mutate(n = ifelse(n < 1, 1, n)) %>% 
    arrange(year, month, desc(T3)) %>% 
    mutate(id = row_number()) %>% 
    filter(id == n) %>% 
    select(year, month, T3, T3_imp) %>% 
    drop_na() %>% 
    mutate(T3_imp = ifelse(T3_imp, 100, 0)) %>% 
    mutate(period = month.abb[month], 
           var = "max") %>% 
    ungroup() %>% 
    select(-month)
  
  d11 <- bind_rows(full_join(miT1, miT2, by = join_by(year, period, var)) %>% 
                     full_join(., miT3, by = join_by(year, period, var)),
                   full_join(maT1, maT2, by = join_by(year, period, var)) %>% 
                     full_join(., maT3, by = join_by(year, period, var)))
  
  # Frost stuff
  
  d12T1 <- tempd %>% 
    filter(year %in% d1$year) %>% 
    group_by(year) %>% 
    mutate(gr = rleid(T1 > 1)) %>% 
    group_by(year, gr) %>% 
    mutate(n = n()) %>% 
    filter(T1 >= 1 & n >= 5) %>% 
    group_by(year) %>% 
    summarise(start_of_season = min(doy),
              end_of_season = max(doy),
              round2(across(T1_imp, ~mean(.x, na.rm = F)*100))) %>% 
    mutate(season_length = end_of_season - start_of_season) %>% 
    pivot_longer(cols = c(start_of_season,end_of_season,season_length),
                 names_to = "var", values_to = "T1") %>% 
    arrange(var,year) %>% 
    mutate(period = "growing_season")
  
  d12T3 <- tempd %>% 
    filter(year %in% d1$year) %>% 
    group_by(year) %>% 
    mutate(gr = rleid(T3 >= 3)) %>% 
    group_by(year, gr) %>% 
    mutate(n = n()) %>% 
    filter(T3 >= 3 & n > 5) %>% 
    group_by(year) %>% 
    summarise(start_of_season = min(doy),
              end_of_season = max(doy),
              round2(across(T3_imp, ~mean(.x, na.rm = F)*100))) %>% 
    mutate(season_length = end_of_season - start_of_season) %>% 
    pivot_longer(cols = c(start_of_season,end_of_season,season_length),
                 names_to = "var", values_to = "T3") %>% 
    arrange(var,year) %>% 
    mutate(period = "growing_season")
  
  d12 <- full_join(d12T1, d12T3, by = join_by(year, var, period))
  
  d13 <- temp %>% 
    group_by(date) %>% 
    summarise(across(c(year,doy,T1,T1_imp:T3_imp), ~mean(.x, na.rm = F)),
              T3 = min(T3)) %>% 
    filter(year %in% d1$year) %>% 
    group_by(year) %>% 
    mutate(gr = rleid(T1 >= 1)) %>% 
    group_by(year, gr) %>% 
    mutate(n = n()) %>% 
    filter(T1 >= 1 & n > 5 & doy <= 212) %>% 
    group_by(year) %>% 
    summarise(across(T3, ~therm_sum(.x, thrh = 0, direc = "below", na.rm = F)),
              round2(across(T3_imp, ~mean(.x, na.rm = F)*100))) %>% 
    drop_na() %>% 
    mutate(period = "growing_season", 
           var = "GST1_frost")
  
  d14 <- temp %>% 
    group_by(date) %>% 
    summarise(across(c(year,doy,T1,T1_imp:T3_imp), ~mean(.x, na.rm = F)),
              T3mean = mean(T3),
              T3 = min(T3)) %>% 
    filter(year %in% d1$year) %>% 
    group_by(year) %>% 
    mutate(gr = rleid(T3mean >= 3)) %>% 
    group_by(year, gr) %>% 
    mutate(n = n()) %>% 
    filter(T3mean >= 3 & n > 5) %>% 
    group_by(year) %>% 
    summarise(across(T3, ~therm_sum(.x, thrh = 0, direc = "below", na.rm = F)),
              round2(across(T3_imp, ~mean(.x, na.rm = F)*100))) %>% 
    drop_na() %>% 
    mutate(period = "growing_season", 
           var = "GST3_frost")
  
  # Summer month frost frequency, excluding times under snow
  
  d15 <- temp %>% 
    filter(year %in% d1$year) %>% 
    filter(month %in% 6:8) %>% 
    group_by(date) %>% 
    summarise(across(c(year,doy,T1,T1_imp:T3_imp), ~mean(.x, na.rm = F)),
              T3 = min(T3)) %>% 
    group_by(year) %>% 
    mutate(gr = rleid(T1 > 1)) %>% 
    group_by(year, gr) %>% 
    mutate(n = n()) %>% 
    filter(T1 >= 1 & n >= 5) %>% 
    group_by(year) %>% 
    summarise(T3 = sum(T3 < 0, na.rm = T),
              round2(across(T3_imp, ~mean(.x, na.rm = F)*100))) %>% 
    mutate(period = "growing_season", 
           var = "summer_frost_freq")
  
  
  # Freeze-thaw cycles
  
  d16T1 <- temp %>%
    group_by(date) %>% 
    summarise(across(c(hydyear,T1_imp), ~mean(.x, na.rm = F)),
              across(c(T1), list(min = ~min(.x, na.rm = F), max = ~max(.x, na.rm = F)), 
                     .names = "{.col}_{.fn}")) %>% 
    pivot_longer(cols = c(T1_min,T1_max), values_to = "T1") %>% 
    arrange(date, hydyear, name) %>% 
    mutate(diff = T1-lag(T1)) %>% 
    mutate(across(T1, ~ifelse(.x >= 0, 1, -1))) %>% 
    group_by(hydyear) %>% 
    mutate(T1 = ifelse(T1*lag(T1) == -1 & abs(diff) > 0.5, 1, 0)) %>% 
    summarise(T1 = sum(T1, na.rm = T),
              round2(across(T1_imp, ~mean(.x, na.rm = F)*100))) %>% 
    drop_na() %>% 
    rename(year = hydyear) %>% 
    mutate(period = "annual", 
           var = "freeze_thaw_freq")
  
  d16T3 <- temp %>%
    group_by(date) %>% 
    summarise(across(c(hydyear,T3_imp), ~mean(.x, na.rm = F)),
              across(c(T3), list(min = ~min(.x, na.rm = F), max = ~max(.x, na.rm = F)), 
                     .names = "{.col}_{.fn}")) %>% 
    pivot_longer(cols = c(T3_min,T3_max), values_to = "T3") %>% 
    arrange(date, hydyear, name) %>% 
    mutate(diff = T3-lag(T3)) %>% 
    mutate(across(T3, ~ifelse(.x >= 0, 1, -1))) %>% 
    group_by(hydyear) %>% 
    mutate(T3 = ifelse(T3*lag(T3) == -1 & abs(diff) > 0.5, 1, 0)) %>% 
    summarise(T3 = sum(T3, na.rm = T),
              round2(across(T3_imp, ~mean(.x, na.rm = F)*100))) %>% 
    drop_na() %>% 
    rename(year = hydyear) %>% 
    mutate(period = "annual", 
           var = "freeze_thaw_freq")
  
  d16 <- full_join(d16T1, d16T3, by = join_by(year, period, var))
  
  # annual mins
  d11aT1 <- d11 %>% 
    filter(var == "min") %>% 
    select(year, T1, T1_imp) %>% 
    group_by(year) %>% 
    arrange(year, T1) %>% 
    mutate(n = n()) %>% 
    slice_head(n = 1) %>% 
    filter(n == 12) %>% 
    mutate(period = "annual", 
           var = "min") %>% 
    select(-n)
  
  d11aT2 <- d11 %>% 
    filter(var == "min") %>% 
    select(year, T2, T2_imp) %>% 
    group_by(year) %>% 
    arrange(year, T2) %>% 
    mutate(n = n()) %>% 
    slice_head(n = 1) %>% 
    filter(n == 12) %>% 
    mutate(period = "annual", 
           var = "min") %>% 
    select(-n)
  
  d11aT3 <- d11 %>% 
    filter(var == "min") %>% 
    select(year, T3, T3_imp) %>% 
    group_by(year) %>% 
    arrange(year, T3) %>% 
    mutate(n = n()) %>% 
    slice_head(n = 1) %>% 
    filter(n == 12) %>% 
    mutate(period = "annual", 
           var = "min") %>% 
    select(-n)
  
  # annual maxs
  d11bT1 <- d11 %>% 
    filter(var == "max") %>% 
    select(year, T1, T1_imp) %>% 
    group_by(year) %>% 
    arrange(year, desc(T1)) %>% 
    mutate(n = n()) %>% 
    slice_head(n = 1) %>% 
    filter(n == 12) %>% 
    mutate(period = "annual", 
           var = "max") %>% 
    select(-n)
  
  d11bT2 <- d11 %>% 
    filter(var == "max") %>% 
    select(year, T2, T2_imp) %>% 
    group_by(year) %>% 
    arrange(year, desc(T2)) %>% 
    mutate(n = n()) %>% 
    slice_head(n = 1) %>% 
    filter(n == 12) %>% 
    mutate(period = "annual", 
           var = "max") %>% 
    select(-n)
  
  d11bT3 <- d11 %>% 
    filter(var == "max") %>% 
    select(year, T3, T3_imp) %>% 
    group_by(year) %>% 
    arrange(year, desc(T3)) %>% 
    mutate(n = n()) %>% 
    slice_head(n = 1) %>% 
    filter(n == 12) %>% 
    mutate(period = "annual", 
           var = "max") %>% 
    select(-n)
  
  d11c <- bind_rows(full_join(d11aT1, d11aT2, by = join_by(year, period, var)) %>% 
                      full_join(., d11aT3, by = join_by(year, period, var)),
                    full_join(d11bT1, d11bT2, by = join_by(year, period, var)) %>% 
                      full_join(., d11bT3, by = join_by(year, period, var)))
  
  # Annual range
  d11d <- d11c %>% 
    group_by(year) %>% 
    summarise(across(c(T1,T2,T3), ~diff(range(.x, na.rm = F))),
              across(c(T1_imp,T2_imp,T3_imp), ~max(.x, na.rm = F))) %>% 
    mutate(period = "annual", 
           var = "range")
  
  dall <- bind_rows(d1,
                    d11c,
                    d11d,
                    d2,
                    d3,
                    d4,
                    d5,
                    d6,
                    d7,
                    d8,
                    d9,
                    d10,
                    d10b,
                    d11,
                    d12,
                    d13,
                    d14,
                    d15,
                    d16) %>% 
    relocate(var, period, year)
  
  dall <- dall %>% 
    rename(T1_value = T1,
           T2_value = T2,
           T3_value = T3) %>% 
    pivot_longer(-c(var, period, year), 
                 names_to = c("sensor", ".value"), 
                 names_sep="_" ) %>% 
    rename(imputed_percentage = imp) %>% 
    mutate(site = siteid) %>% 
    relocate(site) %>% 
    drop_na()
  
  return(dall)
  
}
