# Run this if you want to re-download files

library(tidyverse)


## 1: Read Data ####

## GJB ##

gjb.data <- read_csv("https://raw.githubusercontent.com/theglobaljukebox/cantometrics/master/cldf/data.csv")
gjb.soc <- read_csv("https://raw.githubusercontent.com/theglobaljukebox/cantometrics/master/raw/societies.csv")

## NHS ##

nhs.eth <- read_csv("https://raw.githubusercontent.com/themusiclab/nhs/master/data/nhs/NHSEthnography_AnnotateSec.csv")
nhs.eth.text <- read_csv("https://raw.githubusercontent.com/themusiclab/nhs/master/data/nhs/NHSEthnography_FreeText.csv")
nhs.soc <- read_csv("https://raw.githubusercontent.com/themusiclab/nhs/master/data/nhs/NHSEthnography_Metadata.csv")
nhs.meta <- read_csv("https://raw.githubusercontent.com/themusiclab/nhs/master/data/nhs/NHSCultures_Metadata.csv")

## SCCS ##
sccs.data <- read_csv("https://raw.githubusercontent.com/D-PLACE/dplace-data/master/datasets/SCCS/data.csv")
sccs.soc <- read_csv("https://raw.githubusercontent.com/D-PLACE/dplace-data/master/datasets/SCCS/societies.csv")

## EA ##
ea.data <-  read_csv("https://raw.githubusercontent.com/D-PLACE/dplace-data/master/datasets/EA/data.csv")
ea.soc <-  read_csv("https://raw.githubusercontent.com/D-PLACE/dplace-data/master/datasets/EA/societies.csv")

## 2: Save Data ####

write.csv(gjb.data, 'data/raw/GJB/data.csv')
write.csv(gjb.soc, 'data/raw/GJB/societies.csv')
write.csv(nhs.eth, 'data/raw/NHS/NHSEthnography_AnnotateSec.csv')
write.csv(nhs.eth.text, 'data/raw/NHS/NHSEthnography_FreeText.csv')
write.csv(nhs.soc, 'data/raw/NHS/NHSEthnography_Metadata.csv.csv')
write.csv(nhs.meta, 'data/raw/NHS/NHSCultures_Metadata.csv.csv')
write.csv(sccs.data, 'data/raw/SCCS/data.csv')
write.csv(sccs.soc, 'data/raw/SCCS/societies.csv')
write.csv(ea.data, 'data/raw/EA/data.csv')
write.csv(ea.soc, 'data/raw/EA/societies.csv')
write.csv(ea.subsistence, 'data/raw/EA/embersubsistence_data.csv')
