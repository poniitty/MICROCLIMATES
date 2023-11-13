###############################################################################
# THIS CODE MAKES SOME QUALITY (PRE)CHECKS IF THE SITE AND TOMST IDs LOOKS FINE
#
library(tidyverse)

area <- "RAS"

invisible(lapply(list.files("scr/functions/", ".R$", full.names = T), source))

data_dir <- "/scratch/project_2007415/microclim/2023_data/RAS"

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
  select(site, tomst_id) %>% 
  distinct()

# Any sites with multiple Tomst ids
ids %>% filter(site %in% (ids %>% select(site) %>% filter(duplicated(.)) %>% pull(site)))
# One which should be so...

# Check if any new Tomsts ids in new dataset
f <- list.files(data_dir, pattern = "data_", recursive = T, full.names = T)
fi <- data.frame(file = f)
fi$tomst_id <- unlist(lapply(fi$file, function(x) as.numeric(strsplit(gsub("data_","",rev(strsplit(x, "/")[[1]])[1]), "_")[[1]][1])))

fi %>% filter(!tomst_id %in% unique(ids$tomst_id))
# No, that is good!

# Any dublicated Tomst data files
fi %>% group_by(tomst_id) %>% summarise(n = n()) %>% filter(n > 1) %>% pull(tomst_id) -> doubled_ids
fi %>% filter(tomst_id %in% doubled_ids)
# No, that's good!

# No problems, so good to go!!