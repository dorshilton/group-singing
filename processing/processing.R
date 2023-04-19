suppressPackageStartupMessages({
  library(tidyverse)
  library(lavaan)
  library(semTools)
})


## Read Data ####

## GJB ##

gjb.data <- read.csv('data/raw/GJB/data.csv')
gjb.soc <- read.csv('data/raw/GJB/societies.csv')
gjb.songs <- read.csv('data/raw/GJB/songs.csv')

## NHS ##

nhs.eth <- read.csv('data/raw/NHS/NHSEthnography_AnnotateSec.csv')
nhs.eth.text <- read.csv('data/raw/NHS/NHSEthnography_FreeText.csv')
nhs.soc <- read.csv('data/raw/NHS/NHSEthnography_Metadata.csv')
nhs.meta <- read.csv('data/raw/NHS/NHSCultures_Metadata.csv')

## SCCS ##
sccs.data <- read.csv('data/raw/SCCS/data.csv')
sccs.soc <- read.csv('data/raw/SCCS/societies.csv')

## EA ##
ea.data <-  read.csv('data/raw//EA/data.csv')
ea.soc <-  read.csv('data/raw/EA/societies.csv')


## Process Data ####

## GJB ####

## fix EA id
gjb.soc$ea_id <- gjb.soc$default_DPL_soc_id
gjb.soc[grep('[a-z]\\d+', gjb.soc$ea_id, invert = T), 'ea_id'] <- NA

# unite Japan sample
gjb.soc[grep('Japan', gjb.soc$Division), 'ea_id'] <- 'Ed5'
gjb.soc[gjb.soc$society == 'Hokkaido Japanese', 'ea_id'] <- 'Ed5'

# set Bahia area with ea id corresponding to Bahia Brazilians
gjb.soc[grep('Bahia', gjb.soc$Area), 'ea_id'] <- 'Cf4'
gjb.soc[grep('Sao Paulo', gjb.soc$Area), 'ea_id'] <- NA

## subset only line 1 data
gjb.line_1 <- subset(gjb.data, var_id == "line_1")

# remove row without vocals and wrongly coded as 1
gjb.line_1 <- gjb.line_1 %>% filter(code != 1)

# group singing binary variable - anything other than solo and alternating solo is group
gjb.line_1$group <- ifelse(gjb.line_1$code > 4, 1, 0)

# group score, rated from most emphasis on solo to most integrated group singing
gjb.line_1$group.score <- NA
gjb.line_1$group.score[gjb.line_1$code == 2]  <- 1  # solo
gjb.line_1$group.score[gjb.line_1$code == 4]  <- 1  # solo
gjb.line_1$group.score[gjb.line_1$code == 8]  <- 2  # leader group alternate
gjb.line_1$group.score[gjb.line_1$code == 10] <- 2  # leader group overlap
gjb.line_1$group.score[gjb.line_1$code == 11] <- 2  # group leader overlap
gjb.line_1$group.score[gjb.line_1$code == 9]  <- 3  # group group alternate
gjb.line_1$group.score[gjb.line_1$code == 12] <- 3  # group group overlap
gjb.line_1$group.score[gjb.line_1$code == 6]  <- 4  # unison
gjb.line_1$group.score[gjb.line_1$code == 5]  <- 4  # unison
gjb.line_1$group.score[gjb.line_1$code == 13] <- 5  # interlock

# insert number of songs
gjb.soc <- gjb.line_1 %>%
  distinct(song_id, .keep_all = T) %>%
  group_by(society_id) %>%
  summarise(n = n()) %>%
  left_join(gjb.soc, ., by = 'society_id')

# insert number of group songs
gjb.soc <- gjb.line_1 %>%
  distinct(song_id, group, .keep_all = T) %>%
  group_by(society_id) %>%
  summarise(group_songs = sum(group)) %>%
  left_join(gjb.soc, ., by = 'society_id')

# calculate proportion of group singing
gjb.soc <- gjb.soc %>% 
  mutate(prop_group = group_songs/n)

# code contexts in GJB
gjb.songs$Religious <- 0
gjb.songs$Religious[grep('religious|ritual|rite|pray|initiation|spirit|shaman',gjb.songs$Song, ignore.case = T)] <- 1
gjb.songs$Religious[grep('religious|ritual|rite|pray|initiation|spirit|shaman',gjb.songs$Genre, ignore.case = T)] <- 1
gjb.songs$Religious[grep('religious|ritual|rite|pray|initiation|spirit|shaman',gjb.songs$Song_notes, ignore.case = T)] <- 1

gjb.songs$Dance <- 0
gjb.songs$Dance[grep('dance|dancing',gjb.songs$Song, ignore.case = T)] <- 1
gjb.songs$Dance[grep('dance|dancing',gjb.songs$Genre, ignore.case = T)] <- 1
gjb.songs$Dance[grep('dance|dancing',gjb.songs$Song_notes, ignore.case = T)] <- 1

gjb.songs$Mourning <- 0
gjb.songs$Mourning[grep('mourn|lament|funera',gjb.songs$Song, ignore.case = T)] <- 1
gjb.songs$Mourning[grep('mourn|lament|funera',gjb.songs$Genre, ignore.case = T)] <- 1
gjb.songs$Mourning[grep('mourn|lament|funera',gjb.songs$Song_notes, ignore.case = T)] <- 1

gjb.songs$Healing <- 0
gjb.songs$Healing[grep('heal|curing|cure',gjb.songs$Song, ignore.case = T)] <- 1
gjb.songs$Healing[grep('heal|curing|cure',gjb.songs$Genre, ignore.case = T)] <- 1
gjb.songs$Healing[grep('heal|curing|cure',gjb.songs$Song_notes, ignore.case = T)] <- 1

gjb.songs$Lullaby <- 0
gjb.songs$Lullaby[grep('lullab',gjb.songs$Song, ignore.case = T)] <- 1
gjb.songs$Lullaby[grep('lullab',gjb.songs$Genre, ignore.case = T)] <- 1
gjb.songs$Lullaby[grep('lullab',gjb.songs$Song_notes, ignore.case = T)] <- 1

gjb.context <- gjb.songs %>% 
  select(song_id, society_id, Religious, Dance, Mourning, Healing, Lullaby) %>% 
  filter(Religious == 1 | Dance == 1 | Mourning == 1 | Healing == 1 | Lullaby == 1)

gjb.context <- gjb.line_1 %>% 
  distinct(song_id, group) %>% 
  left_join(gjb.context, ., multiple = 'all', by = 'song_id')

## NHS ####

## Corrections
# fix society names
nhs.soc[nhs.soc$id_hraf == "FE12", "society"] <- "Akan"
nhs.soc[nhs.soc$id_hraf == "RY02", "society"] <- "Chukchee"
nhs.soc[nhs.soc$id_hraf == "MW11", "society"] <- "Shluh"
nhs.soc[nhs.soc$id_hraf == "OA19", "society"] <- "Ifugao"
nhs.soc[nhs.soc$id_hraf == "RV02", "society"] <- "Yakut"
# EA id corrections
# Ojibwa have several ea id's assigned - most appropriate is Na36
nhs.meta[nhs.meta$id_nhs == "NHS-C042", "id_ea"] <- "Na36"
# Guarani
nhs.meta[nhs.meta$id_nhs == "NHS-C033", "id_ea"] <- "Sh7"
# fix latitude longitude data
# Guarani
nhs.soc[nhs.soc$id_hraf == 'SM04', 'latitude'] <- -20
nhs.soc[nhs.soc$id_hraf == 'SM04', 'longitude'] <- -63
# Bahia Brazilians
nhs.soc[nhs.soc$id_hraf == 'SO11', 'latitude'] <- -13
nhs.soc[nhs.soc$id_hraf == 'SO11', 'longitude'] <- -38
# Libyan Bedouin
nhs.soc[nhs.soc$id_hraf == 'MT09', 'latitude'] <- 27
nhs.soc[nhs.soc$id_hraf == 'MT09', 'longitude'] <- 17
# Yakut
nhs.soc[nhs.soc$id_hraf == 'RV02', 'latitude'] <- 65
nhs.soc[nhs.soc$id_hraf == 'RV02', 'longitude'] <- 125
# Korea
nhs.soc[nhs.soc$id_hraf == 'AA01', 'latitude'] <- 38
nhs.soc[nhs.soc$id_hraf == 'AA01', 'longitude'] <- 127
# Taiwan Hokkien
nhs.soc[nhs.soc$id_hraf == 'AD05', 'latitude'] <- 24
nhs.soc[nhs.soc$id_hraf == 'AD05', 'longitude'] <- 121

#insert duplicate text info
nhs.eth <- nhs.eth %>% left_join(.,select(nhs.eth.text,indx, text_duplicate), by = 'indx')
# insert the NHS ID into the data frames
nhs.eth$id_nhs <- nhs.eth.text$id_nhs[match(nhs.eth$indx, nhs.eth.text$indx)]
nhs.soc$id_nhs <- nhs.meta$id_nhs[match(nhs.soc$id_hraf, nhs.meta$id_hraf)]
# add ea id to NHS societies
nhs.soc$ea_id <- nhs.meta$id_ea[match(nhs.soc$id_nhs, nhs.meta$id_nhs)]

# Insert detailed region data that matches GJB regions
nhs.soc <- nhs.soc %>%
  mutate(Region = hraf_region) %>% 
  mutate(Region = replace(Region, hraf_subregion == 'North Asia', 'North Eurasia')) %>%
  mutate(Region = replace(Region, society == 'Saami', 'North Eurasia')) %>% 
  mutate(Region = replace(Region, hraf_subregion == 'South Asia', 'South Asia')) %>%
  mutate(Region = replace(Region, hraf_subregion == 'Southeast Asia', 'Southeast Asia')) %>%
  mutate(Region = replace(Region, hraf_subregion == 'East Asia', 'East Asia')) %>%
  mutate(Region = replace(Region, hraf_region == 'Middle East', 'Western Asia')) %>% 
  mutate(Region = replace(Region, hraf_region == 'Middle America and the Caribbean', 'Central America'))

## Group singing
# group singing binary variable
nhs.eth$group <- ifelse(nhs.eth$singers_n == 'Solo singer', 0, 1)
nhs.eth$group[nhs.eth$singers_n == '.'] <- NA
# remove duplicates and group singing NA
nhs.eth <- nhs.eth %>% filter(text_duplicate == 0 & !is.na(group))

# get proportion of group singing for every society
nhs.soc <- nhs.eth %>%  
  group_by(id_nhs) %>% 
  summarise(n = n(), group_txt = sum(group)) %>% 
  mutate(prop_group = group_txt/n) %>% 
  left_join(nhs.soc, ., by = 'id_nhs')

# NHS context coding

contexts <- c('Religious',
              'Dance',
              'Mourning',
              'Healing',
              'Games',
              'Spectacles',
              'Marriage',
              'Infant care',
              'Ceremonies',
              'Storytelling',
              'Work')

nhs.context <- nhs.eth %>% 
  mutate(context = 
           recode(context1,
                  'Religious practices (religious experiences, prayers, sacrifices, purification, divination)' = 'Religious',
                  'Death (burials, funerals, mourning)' = 'Mourning',
                  'Sickness, medical care, and shamans' = 'Healing',
                  'Infancy and childhood' = 'Infant care',
                  'Cultural identity and pride' = 'Ceremonies',
                  'Agriculture' = 'Work',
                  'Labor' = 'Work')) %>%
  left_join(select(nhs.eth.text, indx, kf_context), by = 'indx') %>% 
  mutate(context = replace(context, grepl('story',kf_context), 'Storytelling')) %>%
  filter(context %in% contexts) %>% 
  select(indx, id_nhs, context, group) 

## EA ####

ea.data <- ea.data %>% 
  filter(var_id == 'EA031') %>% 
  select(soc_id, var_id, code)

ea <- pivot_wider(ea.data, names_from = var_id, values_from = code) %>% 
  rename(ea_id = soc_id)

# standardize EA031
ea$std_EA031 = ea$EA031 / max(ea$EA031, na.rm = TRUE)

nhs.soc <- nhs.soc %>% left_join(ea, by = 'ea_id')

gjb.soc <- gjb.soc %>% left_join(ea, by = 'ea_id')

## SCCS ####

sccs.data <- sccs.data %>% 
  filter(var_id == 'SCCS149' |
           var_id == 'SCCS150' |
           var_id == 'SCCS151' |
           var_id == 'SCCS152' |
           var_id == 'SCCS153' |
           var_id == 'SCCS154' |
           var_id == 'SCCS155' |
           var_id == 'SCCS156' |
           var_id == 'SCCS157' |
           var_id == 'SCCS158') %>% 
  select(soc_id, var_id, code)

sccs <- pivot_wider(sccs.data, names_from = var_id, values_from = code)

sccs.soc <- sccs.soc %>% left_join(sccs,.,by = c('soc_id' = 'id'))

# standardize SCCS variables
sccs.soc$sccs149.writing.s <- sccs.soc$SCCS149 / max(sccs.soc$SCCS149, na.rm = TRUE)
sccs.soc$sccs150.residence.s <- sccs.soc$SCCS150 / max(sccs.soc$SCCS150, na.rm = TRUE)
sccs.soc$sccs151.agriculture.s <- sccs.soc$SCCS151 / max(sccs.soc$SCCS151, na.rm = TRUE)
sccs.soc$sccs152.urbanization.s <- sccs.soc$SCCS152 / max(sccs.soc$SCCS152, na.rm = TRUE)
sccs.soc$sccs153.tech_spec.s <- sccs.soc$SCCS153 / max(sccs.soc$SCCS153, na.rm = TRUE)
sccs.soc$sccs154.transport.s <- sccs.soc$SCCS154 / max(sccs.soc$SCCS154, na.rm = TRUE)
sccs.soc$sccs155.money.s <- sccs.soc$SCCS155 / max(sccs.soc$SCCS155, na.rm = TRUE)
sccs.soc$sccs156.dens_pop.s <- sccs.soc$SCCS156 / max(sccs.soc$SCCS156, na.rm = TRUE)
sccs.soc$sccs157.pol_int.s <- sccs.soc$SCCS157 / max(sccs.soc$SCCS157, na.rm = TRUE)
sccs.soc$sccs158.strat.s <- sccs.soc$SCCS158 / max(sccs.soc$SCCS158, na.rm = TRUE)

# insert xd_id into GJB and NHS
ea_sccs.soc <- merge(ea.soc,sccs.soc, by = "xd_id")
gjb.soc$xd_id <- ea_sccs.soc$xd_id[match(gjb.soc$ea_id, ea_sccs.soc$id)]
nhs.soc$xd_id <- ea_sccs.soc$xd_id[match(nhs.soc$ea_id, ea_sccs.soc$id)]

nhs.soc <- sccs.soc %>% select_if(grepl('xd_id|sccs',names(.))) %>% left_join(nhs.soc,., by = 'xd_id')
gjb.soc <- sccs.soc %>% select_if(grepl('xd_id|sccs',names(.))) %>% left_join(gjb.soc,., by = 'xd_id')


## Social complexity latent variables ####

## SCCS: comparing latent variable models of social complexity ##

# Model 1: social complexity as single dimension
sc1 <- 'comp =~ sccs149.writing.s +
  sccs150.residence.s +
  sccs151.agriculture.s +
  sccs152.urbanization.s +
  sccs153.tech_spec.s +
  sccs154.transport.s +
  sccs155.money.s +
  sccs156.dens_pop.s +
  sccs157.pol_int.s +
  sccs158.strat.s'

# Model 2: social complexity as two dimensions:
# TSD = Social differentiation
# RI = population size and agriculture
sc2 <- 'tsd =~  sccs149.writing.s +
  sccs153.tech_spec.s +
  sccs154.transport.s +
  sccs155.money.s +
  sccs157.pol_int.s +
  sccs158.strat.s
  
  ri =~  sccs150.residence.s +
  sccs151.agriculture.s +
  sccs152.urbanization.s +
  sccs156.dens_pop.s'

# Model 3: social complexity as previous two dimensions, removing land transport
sc3 <- 'tsd =~  sccs149.writing.s +
  sccs153.tech_spec.s +
  sccs155.money.s +
  sccs157.pol_int.s +
  sccs158.strat.s
  
  ri =~  sccs150.residence.s +
  sccs151.agriculture.s +
  sccs152.urbanization.s +
  sccs156.dens_pop.s'

# fit models
fitsc1 <- cfa(sc1, 
              data = sccs.soc, 
              ordered = c('sccs149.writing.s',
                          'sccs150.residence.s',
                          'sccs151.agriculture.s',
                          'sccs152.urbanization.s',
                          'sccs153.tech_spec.s',
                          'sccs154.transport.s',
                          'sccs155.money.s',
                          'sccs156.dens_pop.s',
                          'sccs157.pol_int.s',
                          'sccs158.strat.s'))

fitsc2 <- cfa(sc2, 
              data = sccs.soc, 
              ordered = c('sccs149.writing.s',
                          'sccs153.tech_spec.s',
                          'sccs154.transport.s',
                          'sccs155.money.s',
                          'sccs157.pol_int.s',
                          'sccs158.strat.s',
                          'sccs150.residence.s',
                          'sccs151.agriculture.s',
                          'sccs152.urbanization.s',
                          'sccs156.dens_pop.s'))

fitsc3 <- cfa(sc3, 
              data = sccs.soc, 
              ordered = c('sccs149.writing.s',
                          'sccs153.tech_spec.s',
                          'sccs155.money.s',
                          'sccs157.pol_int.s',
                          'sccs158.strat.s',
                          'sccs150.residence.s',
                          'sccs151.agriculture.s',
                          'sccs152.urbanization.s',
                          'sccs156.dens_pop.s'))

# two dimensions (sc2) - significantly better fit
summary(compareFit(fitsc1, fitsc2))

# sc3 meets standard rmsea and srmr criteria, sc2 does not
# sc2 cfi 0.987 rmsea 0.108 srmr 0.089
fitMeasures(fitsc2, c("cfi","rmsea", "srmr"))
# sc3 cfi 0.998 rmsea 0.047 srmr 0.054
fitMeasures(fitsc3, c("cfi","rmsea", "srmr"))

# predict tsd (social differentiation) and ri (population size and agriculture) values 
# insert into full sccs sample
sccs.soc.sc = cbind(sccs.soc, lavPredict(fitsc3))

# insert tsd and ri to GJB and NHS
gjb.soc <- sccs.soc.sc %>% 
  select(xd_id, tsd, ri) %>% 
  left_join(gjb.soc,., by = 'xd_id')

nhs.soc <- sccs.soc.sc %>% 
  select(xd_id, tsd, ri) %>% 
  left_join(nhs.soc,., by = 'xd_id')

# GJB: merge societies with same ea_id
gjb.soc.ea <- gjb.soc %>% 
  filter(!is.na(ea_id)) %>% 
  select(Society_latitude,
         Society_longitude,
         ea_id,
         n,
         prop_group,
         std_EA031,
         tsd,
         ri) %>% 
  group_by(ea_id) %>% 
  summarise(across(where(is.double), \(x) mean(x, na.rm = TRUE), .names = '{.col}'), n = sum(n, na.rm = T))

gjb.soc.ea <- gjb.soc %>% 
  filter(!is.na(ea_id)) %>% 
  select(ea_id, Region, xd_id, EA031) %>% 
  distinct(ea_id, .keep_all = T) %>% 
  left_join(gjb.soc.ea,.,by = 'ea_id')

## Write processed data ####

write.csv(gjb.line_1, 'data/processed/gjb_line_1.csv', row.names = FALSE)
write.csv(gjb.soc, 'data/processed/gjb_soc.csv', row.names = FALSE)
write.csv(gjb.soc.ea, 'data/processed/gjb_soc_ea.csv', row.names = FALSE)
write.csv(gjb.context, 'data/processed/gjb_context.csv', row.names = FALSE)
write.csv(nhs.eth, 'data/processed/nhs_eth.csv', row.names = FALSE)
write.csv(nhs.context, 'data/processed/nhs_context.csv', row.names = FALSE)
write.csv(nhs.soc, 'data/processed/nhs_soc.csv', row.names = FALSE)
write.csv(sccs.soc, 'data/processed/sccs_soc.csv', row.names = FALSE)