###############################################################################
# THIS CODE MAKES SOME QUALITY (PRE)CHECKS IF THE SITE AND TOMST IDs LOOKS FINE
#
library(tidyverse)

area <- "OUL"

invisible(lapply(list.files("scr/functions/", ".R$", full.names = T), source))

data_dir <- "/scratch/project_2007415/microclim/2023_data/OUL"

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
ids <- read_csv(paste0("data/",area,"/reading_times_",area,".csv")) %>% 
  group_by(site) %>% 
  arrange(site, desc(maxdt)) %>% 
  slice_head(n = 1)

# Check if any new Tomsts ids in new dataset
f <- list.files(data_dir, pattern = "data_", recursive = T, full.names = T)
fi <- data.frame(file = f)
fi$site1 <- toupper(unlist(lapply(fi$file, function(x) rev(strsplit(x, "/")[[1]])[2])))
fi$tomst_id <- unlist(lapply(fi$file, function(x) as.numeric(strsplit(gsub("data_","",rev(strsplit(x, "/")[[1]])[1]), "_")[[1]][1])))

fim <- full_join(fi, ids)

fim %>% filter(is.na(site1)) # None, GOOOOOOD!!!!!!!!!

# Any site name conflicts
fim %>% filter(site1 != site & (!is.na(site)))
# 

fim %>% filter(is.na(file)) # These should be fine

fim %>% filter(!is.na(file)) %>% select(site1) %>% filter(duplicated(.)) # None, GOOD!!!

# No problems, so good to go!!