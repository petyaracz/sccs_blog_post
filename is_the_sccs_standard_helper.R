########################################################################################################
# pull in EA data from dplace github repo, tidy it up, add region and language family information, the latter from the dplace # html, the former from wherever. write results into ea_tidy.csv
#
########################################################################################################


library("RCurl") # to connect to the informational superhighway
library("dplyr") # to massage data
library("stringr") # to edit strings
library("reshape2") # to massage data more
library("jsonlite") # to get the json

pullInEA <- function(){
# Ethno Atlas from dplace github. accessed 12/7/17
variables <- read.csv(text=getURL("https://raw.githubusercontent.com/D-PLACE/dplace-data/master/datasets/EA/variables.csv"), header=T)
codes <- read.csv(text=getURL("https://raw.githubusercontent.com/D-PLACE/dplace-data/master/datasets/EA/codes.csv"), header=T)
dat <- read.csv(text=getURL("https://raw.githubusercontent.com/D-PLACE/dplace-data/master/datasets/EA/data.csv"), header=T) # data is a reserved word
societies <- read.csv(text=getURL("https://raw.githubusercontent.com/D-PLACE/dplace-data/master/datasets/EA/societies.csv"), header=T) %>% select(id, glottocode, pref_name_for_society, Lat, Long) %>% rename(lat = Lat, lon = Long, soc_id = id, society = pref_name_for_society)
locations <- getURL("https://raw.githubusercontent.com/D-PLACE/dplace-data/master/geo/societies_tdwg.json") %>% fromJSON
locations2 <- do.call(rbind.data.frame, locations)
locations2$soc_id <- rownames(locations2)  
locations2 <- subset(locations2, soc_id %in% dat$soc_id) %>% select(-code) %>% rename(region = name)

languages <- read.csv(text=getURL("https://raw.githubusercontent.com/D-PLACE/dplace-data/master/csv/glottolog.csv"), header=T)
languages <- languages %>% rename(family = family_name, glottocode = id) %>% select(family, glottocode)

societies <- merge(societies,languages)

# get society list from sccs

sccs <- read.csv(text=getURL("https://raw.githubusercontent.com/D-PLACE/dplace-data/master/datasets/SCCS/societies.csv"), header=T) %>% select(pref_name_for_society,glottocode,HRAF_name_ID)
societies$in_sccs <- ifelse(societies$glottocode %in% sccs$glottocode, T, F)

variables2 <- variables[,c('id','category','title','definition','type')]
variables2 <- rename(variables2, var_id = id)
variables3 <- merge(variables2,codes)
# adds up
dat2 <- dat[,c('soc_id','var_id','code')]
dat3 <- merge(dat2,variables3)
# doesn't add up: for some reason pop size (EA202) drops out. submitted an Issue on github. in the meantime
dat3b <- dat2[dat2$var_id == 'EA202',]
merge_with_dat3b <- variables3[variables3$var_id == 'EA202',] %>% select(-code)
dat3b2 <- merge(dat3b, merge_with_dat3b)
dat4 <- rbind(dat3,dat3b2)
# still doesn't add up, now there are a couple hundred extra rows. 
# > nrow(dat2[dat2$var_id == 'EA202',])
# [1] 1291
# > nrow(dat4[dat4$var_id == 'EA202',])
# [1] 1629
dat5 <- unique(dat4)
# right.

# add regions and long / lat
dat6 <- merge(dat5,locations2)

# add lang fam
dat7 <- merge(societies,dat6)
}

dat7 <- pullInEA()

write.csv(dat7, file = 'ea_tidy.csv', row.names = F)