###############################################################################
# THIS CODE MAKES SOME QUALITY (PRE)CHECKS IF THE SITE AND TOMST IDs LOOKS FINE
#
library(tidyverse)

area <- "PAL"

invisible(lapply(list.files("scr/functions/", ".R$", full.names = T), source))

data_dir <- "/scratch/project_2007415/DATA2023/Pallas2023"

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
# 
f <- list.files(data_dir, pattern = "^data_", recursive = T, full.names = T)
fi <- data.frame(file = f)
fi$site <- toupper(unlist(lapply(fi$file, function(x) rev(strsplit(x, "/")[[1]])[2])))
fi$tomst_id <- unlist(lapply(fi$file, function(x) as.numeric(strsplit(gsub("data_","",rev(strsplit(x, "/")[[1]])[1]), "_")[[1]][1])))

fi %>% select(tomst_id) %>% duplicated() %>% sum # No duplicated tomst ids
fi %>% select(site) %>% duplicated() %>% sum # No duplicated site ids

fi$site # All site names looks good

fi %>% 
  select(site, tomst_id) %>% 
  mutate(site = add_zeros(site, area)) %>% 
  arrange(site) %>% 
  write_csv(paste0("data/",area,"/initial_site_codes.csv"))

# No problems, so good to go!!