###############################################################################
# THIS CODE MAKES SOME QUALITY (PRE)CHECKS IF THE SITE AND TOMST IDs LOOKS FINE
#
library(tidyverse)

area <- "HYY"

invisible(lapply(list.files("scr/functions/", ".R$", full.names = T), source))

data_dir <- "/scratch/project_2007415/DATA2023/HYY2023"

# Create needed folders if not exists
if(!dir.exists(paste0("data/",area))){
  dir.create(paste0("data/",area))
}
if(!dir.exists(paste0("output/",area))){
  dir.create(paste0("output/",area))
}
if(!dir.exists(paste0("scr/",area))){
  dir.create(paste0("scr/",area))
}

# List binary and command files to be removed from repository if also data file exists

f <- c(list.files(data_dir, pattern = "binary_", recursive = T, full.names = T),
       list.files(data_dir, pattern = "command_", recursive = T, full.names = T))

for(i in f){ 
  if(file.exists(gsub("binary_","data_",i)) | file.exists(gsub("command_","data_",i))){
    unlink(i)
  } else {
    print(paste0("DATA FILE MISSING!!! ", i))
  } 
}

###########################################################################
# Check Tomst ID-numbers from last year data

# bind_rows(read_csv("/projappl/project_2003061/repos/microclim_hyytiala/data/reading_times_2020.csv"),
#           read_csv("/projappl/project_2003061/repos/microclim_hyytiala/data/reading_times_2021.csv"),
#           read_csv("/projappl/project_2003061/repos/microclim_hyytiala/data/reading_times_2022.csv")) %>% 
#   distinct() %>% 
#   mutate(site = add_zeros(site, area)) %>% 
#   write_csv(paste0("data/",area,"/reading_times_",area,".csv"))

ids <- read_csv(paste0("data/",area,"/reading_times_",area,".csv")) %>% 
  select(site, tomst_id) %>% 
  distinct()

# Check if any new Tomsts ids in new dataset
f <- list.files(data_dir, pattern = "data_", recursive = T, full.names = T)
fi <- data.frame(file = f)
fi$tomst_id <- unlist(lapply(fi$file, function(x) as.numeric(strsplit(gsub("data_","",rev(strsplit(x, "/")[[1]])[1]), "_")[[1]][1])))

fi %>% filter(!tomst_id %in% unique(ids$tomst_id))
# Yes, two! Add these manually to the ids file
ids <- read_csv(paste0("data/",area,"/reading_times_",area,".csv"))
bind_rows(ids,
          tibble(site = c("HYY016", "HYY025", "HYY053"),
                 maxdt = as_datetime(rep("2000-11-22 12:00:00",times=3), tz = tz(ids$maxdt)),
                 tomst_id = c(95224054,95224051,95224061))) %>% 
  distinct() %>% 
  write_csv(paste0("data/",area,"/reading_times_",area,".csv"))


# try the same with updated file
ids <- read_csv(paste0("data/",area,"/reading_times_",area,".csv")) %>% 
  select(site, tomst_id) %>% 
  distinct()

# Check if any new Tomsts ids in new dataset
f <- list.files(data_dir, pattern = "data_", recursive = T, full.names = T)
fi <- data.frame(file = f)
fi$tomst_id <- unlist(lapply(fi$file, function(x) as.numeric(strsplit(gsub("data_","",rev(strsplit(x, "/")[[1]])[1]), "_")[[1]][1])))

fi %>% filter(!tomst_id %in% unique(ids$tomst_id))
# None! Good!

# Any sites with multiple Tomst ids
ids %>% filter(site %in% (ids %>% select(site) %>% filter(duplicated(.)) %>% pull(site)))
# These are fine


# Any dublicated Tomst data files
fi %>% group_by(tomst_id) %>% summarise(n = n()) %>% filter(n > 1) %>% pull(tomst_id) -> doubled_ids
fi %>% filter(tomst_id %in% doubled_ids)
# No, that's good!

# Any conflicting site names with the folder structure
f <- list.files(data_dir, pattern = "data_", recursive = T, full.names = T)
fi <- data.frame(file = f)
fi$site1 <- toupper(unlist(lapply(fi$file, function(x) rev(strsplit(x, "/")[[1]])[2])))
fi$tomst_id <- unlist(lapply(fi$file, function(x) as.numeric(strsplit(gsub("data_","",rev(strsplit(x, "/")[[1]])[1]), "_")[[1]][1])))

fi <- fi %>% mutate(site1 = ifelse(nchar(site1) == 8, NA, site1))

fim <- full_join(fi, ids %>% rename(site2 = site)) %>% 
  mutate(site1 = ifelse(is.na(site1), site2, site1)) %>% 
  mutate(site1 = add_zeros(site1, area))

fim %>% filter(is.na(site1)) # None, GOOOOOOD!!!!!!!!!

# Any site name conflicts
fim %>% filter(site1 != site2 & (!is.na(site2)))
# These ones are fine and handled

# No problems no more, so good to go!!