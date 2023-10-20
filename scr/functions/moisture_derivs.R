
moisture_derivs <- function(df_micro, quant){
  library(tidyverse)
  library(lubridate)
  library(scales)
  library(zoo)
  library(broom)
  
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
  
  dall <- lapply(sites, moisture_derivs_inner, df_micro = df_micro, quant = quant)
  
  dall <- dall %>% 
    bind_rows()
  
  return(dall)
}

cv_sd <- function(x, na.rm = T){
  return(sd(x, na.rm = na.rm)/mean(x, na.rm = na.rm))
}
cv_mad <- function(x, na.rm = T){
  return(mad(x, na.rm = na.rm)/mean(x, na.rm = na.rm))
}
cv_var <- function(x, na.rm = T){
  return(var(x, na.rm = na.rm)/mean(x, na.rm = na.rm))
}
qcd <- function(x, na.rm = T){
  return(diff(quantile(x, c(0.25,0.75), na.rm = na.rm))/sum(quantile(x, c(0.25,0.75), na.rm = na.rm)))
}

moisture_derivs_inner <- function(siteid, df_micro, quant){
  # siteid <- "RAS001"
  print(siteid)
  
  temp <- df_micro %>% filter(site == siteid | is.na(site)) %>% 
    arrange(date, datetime) %>% 
    mutate(moistsat = rescale_max(moist, to = c(0,100)))
  
  tempd <- temp %>% 
    group_by(date) %>% 
    summarise(across(c(year,hydyear,month,doy,hyddoy,moistsat,T1:moist_imp), ~mean(.x, na.rm = F)))
  
  
  # Annual means
  comp_years <- temp %>% 
    group_by(year) %>% 
    summarise(T1 = mean(T1, na.rm = F)) %>% 
    drop_na() %>% 
    select(-T1)
  
  comp_months <- temp %>% 
    group_by(year, month) %>% 
    summarise(T1 = mean(T1, na.rm = F),
              .groups = "drop") %>% 
    drop_na() %>% 
    select(-T1)
  
  funcs <- list(mean = ~mean(.x, na.rm = T),
                median = ~median(.x, na.rm = T),
                max = ~mean(.x, na.rm = T),
                min = ~min(.x, na.rm = T),
                sd = ~sd(.x, na.rm = T),
                var = ~var(.x, na.rm = T),
                mad = ~mad(.x, na.rm = T),
                cvsd = ~cv_sd(.x, na.rm = T),
                cvvar = ~cv_var(.x, na.rm = T),
                cvmad = ~cv_mad(.x, na.rm = T),
                qcd = ~qcd(.x, na.rm = T),
                iqr50 = ~as.numeric(diff(quantile(.x, c(0.25,0.75), na.rm = T))),
                iqr90 = ~as.numeric(diff(quantile(.x, c(0.05,0.95), na.rm = T))))
  
  # Monthly variables
  
  d1 <- tempd %>% 
    group_by(year, month) %>% 
    mutate(na_prop = mean(is.na(moist))) %>% 
    filter(na_prop < 0.5) %>% 
    summarise(across(c(moist,moistsat), funcs),
              round2(across(moist_imp, ~mean(.x, na.rm = T)*100)),
              .groups = "drop") %>% 
    drop_na() %>% 
    mutate(period = month.abb[month]) %>% 
    select(-month) %>% 
    pivot_longer(-c(period, year, moist_imp), 
                 names_to = c("moist_type", ".value"), 
                 names_sep="_" ) %>% 
    pivot_longer(-c(period, year, moist_imp, moist_type),
                 names_to = "var", values_to = "value")
  
  # Ice free season, i.e., soil T >= 1C
  
  d2 <- tempd %>% 
    filter(year %in% comp_years$year) %>% 
    group_by(year) %>% 
    summarise(across(c(moist,moistsat), funcs),
              round2(across(moist_imp, ~mean(.x, na.rm = T)*100))) %>% 
    drop_na() %>% 
    mutate(period = "ice_free_season") %>% 
    pivot_longer(-c(period, year, moist_imp), 
                 names_to = c("moist_type", ".value"), 
                 names_sep="_" ) %>% 
    pivot_longer(-c(period, year, moist_imp, moist_type),
                 names_to = "var", values_to = "value")
  
  # Growing season
  
  gsl <- tempd %>% 
    filter(year %in% d1$year) %>% 
    group_by(year) %>% 
    mutate(gr = rleid(T3 >= 3)) %>% 
    group_by(year, gr) %>% 
    mutate(n = n()) %>% 
    filter(T3 >= 3 & n > 5) %>% 
    group_by(year) %>% 
    summarise(start_of_season = min(doy),
              end_of_season = max(doy)) %>% 
    filter(year %in% comp_years$year)
  
  d3 <- tempd %>% 
    right_join(., gsl, by = join_by(year)) %>% 
    filter(doy >= start_of_season & doy <= end_of_season) %>% 
    group_by(year) %>% 
    summarise(across(c(moist,moistsat), funcs),
              round2(across(moist_imp, ~mean(.x, na.rm = T)*100))) %>% 
    drop_na() %>% 
    mutate(period = "growing_season") %>% 
    pivot_longer(-c(period, year, moist_imp), 
                 names_to = c("moist_type", ".value"), 
                 names_sep="_" ) %>% 
    pivot_longer(-c(period, year, moist_imp, moist_type),
                 names_to = "var", values_to = "value")
  
  # Driest and wettest two weeks (13 days) during the ice free season
  rolled <- tempd %>% 
    select(year, doy, moist, moistsat, moist_imp) %>% 
    filter(year %in% comp_years$year) %>% 
    group_by(year) %>% 
    drop_na() %>% 
    mutate(across(c(moist,moistsat,moist_imp), ~rollmean(.x, 13, fill = NA, na.rm = T, align = "center"))) %>% 
    mutate(moist_imp = moist_imp*100) %>% 
    drop_na()
  
  d4 <- bind_rows(rolled %>% arrange(year, moist) %>% slice_head(n = 1) %>% 
                    pivot_longer(c(moist, moistsat), names_to = "moist_type", values_to = "value") %>% 
                    mutate(var = "driest_two_weeks") %>% select(-doy),
                  rolled %>% arrange(year, moist) %>% slice_head(n = 1) %>% 
                    pivot_longer(c(moist, moistsat), names_to = "moist_type", values_to = "value2") %>% 
                    rename(value = doy) %>% select(-value2) %>% 
                    mutate(var = "driest_two_weeks_timing"),
                  rolled %>% arrange(year, desc(moist)) %>% slice_head(n = 1) %>% 
                    pivot_longer(c(moist, moistsat), names_to = "moist_type", values_to = "value") %>% 
                    mutate(var = "wettest_two_weeks") %>% select(-doy),
                  rolled %>% arrange(year, desc(moist)) %>% slice_head(n = 1) %>% 
                    pivot_longer(c(moist, moistsat), names_to = "moist_type", values_to = "value2") %>% 
                    rename(value = doy) %>% select(-value2) %>% 
                    mutate(var = "wettes_two_weeks_timing")) %>% 
    ungroup() %>% 
    mutate(period = "ice_free_season")
  
  # Driest and wettest two weeks (13 days) during the growing season
  rolled <- tempd %>% 
    right_join(., gsl, by = join_by(year)) %>% 
    filter(doy >= start_of_season & doy <= end_of_season) %>% 
    select(year, doy, moist, moistsat, moist_imp) %>% 
    filter(year %in% comp_years$year) %>% 
    group_by(year) %>% 
    drop_na() %>% 
    mutate(across(c(moist,moistsat,moist_imp), ~rollmean(.x, 13, fill = NA, na.rm = T, align = "center"))) %>% 
    mutate(moist_imp = moist_imp*100) %>% 
    drop_na()
  
  d5 <- bind_rows(rolled %>% arrange(year, moist) %>% slice_head(n = 1) %>% 
                    pivot_longer(c(moist, moistsat), names_to = "moist_type", values_to = "value") %>% 
                    mutate(var = "driest_two_weeks") %>% select(-doy),
                  rolled %>% arrange(year, moist) %>% slice_head(n = 1) %>% 
                    pivot_longer(c(moist, moistsat), names_to = "moist_type", values_to = "value2") %>% 
                    rename(value = doy) %>% select(-value2) %>% 
                    mutate(var = "driest_two_weeks_timing"),
                  rolled %>% arrange(year, desc(moist)) %>% slice_head(n = 1) %>% 
                    pivot_longer(c(moist, moistsat), names_to = "moist_type", values_to = "value") %>% 
                    mutate(var = "wettest_two_weeks") %>% select(-doy),
                  rolled %>% arrange(year, desc(moist)) %>% slice_head(n = 1) %>% 
                    pivot_longer(c(moist, moistsat), names_to = "moist_type", values_to = "value2") %>% 
                    rename(value = doy) %>% select(-value2) %>% 
                    mutate(var = "wettes_two_weeks_timing")) %>% 
    ungroup() %>% 
    mutate(period = "growing_season")
  
  # First two weeks of growing season
  
  d6 <- tempd %>% 
    right_join(., gsl, by = join_by(year)) %>% 
    filter(doy >= start_of_season) %>% 
    group_by(year) %>% 
    slice_head(n = 14) %>% 
    summarise(across(c(moist,moistsat), funcs),
              round2(across(moist_imp, ~mean(.x, na.rm = T)*100))) %>% 
    drop_na() %>% 
    mutate(period = "first_two_weeks_of_GS") %>% 
    pivot_longer(-c(period, year, moist_imp), 
                 names_to = c("moist_type", ".value"), 
                 names_sep="_" ) %>% 
    pivot_longer(-c(period, year, moist_imp, moist_type),
                 names_to = "var", values_to = "value")
  
  # First four weeks of growing season
  
  d7 <- tempd %>% 
    right_join(., gsl, by = join_by(year)) %>% 
    filter(doy >= start_of_season) %>% 
    group_by(year) %>% 
    slice_head(n = 28) %>% 
    summarise(across(c(moist,moistsat), funcs),
              round2(across(moist_imp, ~mean(.x, na.rm = T)*100))) %>% 
    drop_na() %>% 
    mutate(period = "first_four_weeks_of_GS") %>% 
    pivot_longer(-c(period, year, moist_imp), 
                 names_to = c("moist_type", ".value"), 
                 names_sep="_" ) %>% 
    pivot_longer(-c(period, year, moist_imp, moist_type),
                 names_to = "var", values_to = "value")
  
  # Quantiles of the growing season
  d8 <- tempd %>% 
    right_join(., gsl, by = join_by(year)) %>% 
    filter(doy >= start_of_season & doy <= end_of_season) %>% 
    select(year, doy, moist, moistsat, moist_imp) %>% 
    filter(year %in% comp_years$year) %>% 
    group_by(year) %>% 
    drop_na() %>% 
    mutate(gr = rep(c(1,2,3,4), length.out = n()) %>% sort) %>% 
    group_by(year, gr) %>% 
    summarise(across(c(moist,moistsat), funcs),
              round2(across(moist_imp, ~mean(.x, na.rm = T)*100)),
              .groups = "drop") %>% 
    drop_na() %>% 
    mutate(period = factor(gr, levels = c(1,2,3,4), labels = c("1st_quartile_growing_season",
                                                               "2nd_quartile_growing_season",
                                                               "3rd_quartile_growing_season",
                                                               "4th_quartile_growing_season"))) %>% 
    select(-gr) %>% 
    pivot_longer(-c(period, year, moist_imp), 
                 names_to = c("moist_type", ".value"), 
                 names_sep="_" ) %>% 
    pivot_longer(-c(period, year, moist_imp, moist_type),
                 names_to = "var", values_to = "value")
  
  # Trends along the ice-free season
  
  mods <- tempd %>% 
    filter(year %in% comp_years$year) %>% 
    select(year, doy, moist, moist_imp) %>% 
    drop_na() %>% 
    nest(data = -year) %>% 
    mutate(model = map(data, ~lm(moist~doy, data = .)),
           moist_imp = map(data, ~mean(.x$moist_imp, na.rm = T)*100), 
           tidied = map(model, tidy),
           glanced = map(model, glance)) %>% 
    unnest(moist_imp) %>% 
    select(-data, -model)
  
  d9 <- bind_rows(mods %>% unnest(tidied) %>% filter(term == "doy") %>% 
                    select(year, moist_imp, estimate) %>% 
                    mutate(period = "ice_free_season", 
                           var = "trend_slope",
                           moist_type = "moist") %>% 
                    rename(value = estimate),
                  mods %>% unnest(glanced) %>%
                    select(year, moist_imp, r.squared) %>% 
                    mutate(period = "ice_free_season", 
                           var = "trend_r2",
                           moist_type = "moist") %>% 
                    rename(value = r.squared))
  
  # Trends along the growing season
  
  mods <- tempd %>% 
    right_join(., gsl, by = join_by(year)) %>% 
    filter(doy >= start_of_season & doy <= end_of_season) %>% 
    filter(year %in% comp_years$year) %>% 
    select(year, doy, moistsat, moist_imp) %>% 
    drop_na() %>% 
    nest(data = -year) %>% 
    mutate(model = map(data, ~lm(moistsat~doy, data = .)),
           moist_imp = map(data, ~mean(.x$moist_imp, na.rm = T)*100), 
           tidied = map(model, tidy),
           glanced = map(model, glance)) %>% 
    unnest(moist_imp) %>% 
    select(-data, -model)
  
  d10 <- bind_rows(mods %>% unnest(tidied) %>% filter(term == "doy") %>% 
                    select(year, moist_imp, estimate) %>% 
                    mutate(period = "ice_free_season", 
                           var = "trend_slope",
                           moist_type = "moistsat") %>% 
                    rename(value = estimate),
                  mods %>% unnest(glanced) %>%
                    select(year, moist_imp, r.squared) %>% 
                    mutate(period = "ice_free_season", 
                           var = "trend_r2",
                           moist_type = "moistsat") %>% 
                    rename(value = r.squared))
  
  
  # Combine
  
  dall <- bind_rows(d1,
                    d2,
                    d3,
                    d4,
                    d5,
                    d6,
                    d7,
                    d8,
                    d9,
                    d10) %>% 
    relocate(var, period, year)
  
  dall <- dall %>% 
    rename(imputed_percentage = moist_imp) %>% 
    mutate(site = siteid) %>% 
    relocate(site) %>% 
    drop_na()
  
  return(dall)
  
}