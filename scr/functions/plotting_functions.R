
monthly_tomst_plots <- function(siteids, df, visdir = temp_dir, months_to_plot = times){
  print(paste0("Saving the pdf's to: ", visdir))
  
  x <- lapply(siteids, function(siteid){
    # siteid <- sites[1]
    print(paste0("plotting ",siteid, "..."))
    pdf(paste0(visdir, "/monthly_", siteid, ".pdf"), 10, 6)
    temp <- df %>% filter(site == siteid)
    
    if(length(na.omit(unique(temp$tomst_id))) > 1){
      
      for(ii in na.omit(unique(temp$tomst_id))){
        
        for (tt in 1:(length(months_to_plot) - 1)) {
          temp %>% filter(tomst_id == ii) %>%
            filter(datetime >= ymd(months_to_plot[tt]),
                   datetime < ymd(months_to_plot[tt + 1])) -> dft
          
          if(nrow(dft %>% filter(complete.cases(.))) > 0){
            dft %>%
              ggplot(aes(x = datetime)) +
              geom_line(aes(y = T3), col = "cornflowerblue") +
              geom_line(aes(y = T2), col = "brown1") +
              geom_line(aes(y = T1), col = "darkgoldenrod") +
              theme_minimal() +
              ylab("Temperature") + xlab("Date") +
              ggtitle(paste("Site: ", siteid, "; Tomst: ", ii, "; Time: ", months_to_plot[tt])) +
              scale_x_datetime(date_minor_breaks = "1 day") -> GG1
            
            dft %>%
              ggplot(aes(x = datetime)) +
              geom_line(aes(y = moist), col = "blue") +
              theme_minimal() +
              ylab("Moisture") + xlab("Date") +
              ggtitle(paste("Site: ", siteid, "; Time: ", months_to_plot[tt])) +
              scale_x_datetime(date_minor_breaks = "1 day") -> GG2
            
            print(plot_grid(plotlist = list(GG1, GG2), nrow = 2))
          }
        }
      }
    } else {
      for (tt in 1:(length(months_to_plot) - 1)) {
        
        temp %>% 
          filter(datetime >= ymd(months_to_plot[tt]),
                 datetime < ymd(months_to_plot[tt + 1])) -> dft
        tid <- unique(temp$tomst_id)
        if(nrow(dft %>% filter(complete.cases(.))) > 0){
          dft %>%
            ggplot(aes(x = datetime)) +
            geom_line(aes(y = T3), col = "cornflowerblue") +
            geom_line(aes(y = T2), col = "brown1") +
            geom_line(aes(y = T1), col = "darkgoldenrod") +
            theme_minimal() +
            ylab("Temperature") + xlab("Date") +
            ggtitle(paste("Site: ", siteid, "; Tomst: ", tid, "; Time: ", months_to_plot[tt])) +
            scale_x_datetime(date_minor_breaks = "1 day") -> GG1
          
          dft %>%
            ggplot(aes(x = datetime)) +
            geom_line(aes(y = moist), col = "blue") +
            theme_minimal() +
            ylab("Moisture") + xlab("Date") +
            ggtitle(paste("Site: ", siteid, "; Time: ", months_to_plot[tt])) +
            scale_x_datetime(date_minor_breaks = "1 day") -> GG2
          
          print(plot_grid(plotlist = list(GG1, GG2), nrow = 2))
        }
      }
    }
    (dev.off())
  })
}  
