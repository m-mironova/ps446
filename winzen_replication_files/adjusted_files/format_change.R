#This code makes Winzen data usable to transforming it from tab-delineated
#to comma separated file

library(here)
#set wd to relevant top-level directory

#name of df chosen to correspond with replication code
analysisR <- read.delim(here("PS446", "winzen_replication_files", 
                             "analysis.tab")) 

#create new directory to store R-ready files from winzen
dir.create(
  here("PS446", "winzen_replication_files", "adjusted_files"))

#save analysisR as a .csv file
write.csv(analysisR, here("PS446", "winzen_replication_files", 
                          "adjusted_files", "analysisR.csv"))

