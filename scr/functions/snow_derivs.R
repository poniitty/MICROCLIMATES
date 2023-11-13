
snow_derivs <- function(df_micro){
  
  df_micro <- df_micro %>% 
    select(datetime, site, T2, T2_imp) %>% 
    mutate(date = as_date(datetime)) %>% 
    mutate(year = year(date))
  
  df_micro <- df_micro %>% 
    full_join(.,
              tibble(date = seq.Date(as_date(paste0(min(df_micro$year),"-01-01")), as_date(paste0(max(df_micro$year),"-12-31")), by = 1)),
              by = join_by(date)) %>% 
    arrange(site, datetime)
  
  daily <- df_micro %>% 
    group_by(site, date) %>% 
    summarise(across(T2, list(min = ~min(.x, na.rm = F),
                              max = ~max(.x, na.rm = F))),
              T2_imp = mean(T2_imp, na.rm = F)) %>% 
    mutate(year = year(date),
           doy = yday(date)) %>% 
    relocate(year:doy, .after = date)
  
  sites <- unique(daily$site) %>% na.omit %>% sort
  
  dall <- lapply(sites, snow_derivs_inner, daily = daily)
  
  dall <- dall %>% 
    bind_rows()
  
  return(dall)
}

snow_derivs_inner <- function(siteid, daily){
  # siteid <- "RAS073"
  print(siteid)
  temp <- daily %>% filter(site == siteid | is.na(site)) %>% 
    arrange(date)
  
  temp %>% 
    mutate(center_t = rollapply(T2_max, width=9, FUN=max, fill = NA, partial = T, align = "center"),
           center_tmin = rollapply(T2_min, width=9, FUN=min, fill = NA, partial = T, align = "center"),
           lead_t = rollapply(T2_max, width=9, FUN=max, fill = NA, partial = T, align = "right"),
           lead_tmin = rollapply(T2_min, width=9, FUN=min, fill = NA, partial = T, align = "right"),
           lag_t = rollapply(T2_max, width=9, FUN=max, fill = NA, partial = T, align = "left"),
           lag_tmin = rollapply(T2_min, width=9, FUN=min, fill = NA, partial = T, align = "left")) %>% 
    mutate(center_range = center_t-center_tmin,
           lead_range = lead_t-lead_tmin,
           lag_range = lag_t-lag_tmin,
           center_t = ifelse(center_t < 1 & center_range < 10, 1, 0),
           lead_t = ifelse(lead_t < 1 & lead_range < 10, 1, 0),
           lag_t = ifelse(lag_t < 1 & lag_range < 10, 1, 0)) %>% 
    mutate(lead_c = rollapply(center_t, width=5, FUN=max, fill = NA, partial = T, align = "right"),
           lag_c = rollapply(center_t, width=5, FUN=max, fill = NA, partial = T, align = "left")) %>% 
    rowwise() %>% mutate(snow = max(c(lead_c, lag_c))) %>% 
    ungroup() -> dd
  
  # # Plot an example
  # dd %>% 
  #   ggplot(aes_string(x="date")) +
  #   geom_hline(yintercept = 0)+
  #   geom_ribbon(aes(ymin=T2_min, ymax=T2_max), fill = "gray10")+
  #   geom_line(aes(y = lead_t), col = "white", size = 0.8) +
  #   geom_line(aes(y = lag_t), col = "blue", size = 0.8) +
  #   geom_line(aes(y = snow), col = "red", size = 0.8) +
  #   theme_dark() +
  #   ylab("Temperature") + xlab("Date")
  
  midsomdoy <- dd %>% 
    select(date, snow, doy) %>% 
    filter(snow == 0) %>% pull(doy) %>% median %>% round
  
  dd <- dd %>% 
    select(date, snow, doy, T2_imp) %>% 
    mutate(hyddate = date + days(365 - midsomdoy)) %>% 
    mutate(hyddoy = yday(hyddate),
           hydyear = year(hyddate)) %>% 
    select(date, hydyear, doy, hyddoy, snow, T2_imp)
  
  dd <- dd %>% 
    mutate(T2_imp = rollapply(T2_imp, width=15, FUN=mean, fill = NA, partial = F, align = "center")*100)
  
  d1 <- bind_rows(dd %>% 
                    group_by(hydyear) %>% 
                    filter(snow == 1) %>% 
                    slice(1) %>% 
                    drop_na() %>% 
                    mutate(var = "newsnow_doy"),
                  dd %>% 
                    group_by(hydyear) %>% 
                    filter(snow == 1) %>% 
                    slice(n()) %>% 
                    drop_na() %>% 
                    mutate(var = "snowmelt_doy")) %>% 
    rename(year = hydyear,
           value = doy) %>% 
    select(year, var, value, T2_imp) %>% 
    ungroup
  
  # Determine the length of snow periods
  d2 <- dd %>% 
    drop_na() %>% 
    group_by(hydyear) %>% 
    mutate(grp = rleid(snow)) %>% 
    group_by(grp, snow, hydyear) %>% 
    summarise(n = n(),
              T2_imp = mean(T2_imp),
              .groups = "drop") %>% 
    filter(snow == 1) %>% 
    group_by(hydyear) %>% 
    summarise(n_snow_days = sum(n),
              max_snow_period_length = max(n),
              n_snow_period = n(),
              T2_imp = weighted.mean(T2_imp, n)) %>% 
    rename(year = hydyear) %>% 
    pivot_longer(cols = c(n_snow_days, max_snow_period_length, n_snow_period), names_to = "var") %>% 
    filter(year %in% (d1 %>% group_by(year) %>% count %>% filter(n == 2) %>% pull(year)))
  
  d3 <- bind_rows(d1, d2) %>% 
    mutate(site = siteid) %>% 
    relocate(site) %>% 
    arrange(var, year)
  
  return(d3)
}