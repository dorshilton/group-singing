suppressPackageStartupMessages({
  library(tidyverse)
  library(lme4)
  library(lavaan)
  library(semTools)
  library(brms)
  library(bayesplot)
  library(sjPlot)
})

## Descriptive stats ####

## overall predominance of group singing

# GJB 0.65
gjb.line_1 %>% 
  distinct(song_id, .keep_all = T) %>% 
  filter(group == 1) %>% 
  nrow() / nrow(distinct(gjb.line_1, song_id))

# NHS 0.65
nhs.eth %>% 
  filter(group == 1) %>% 
  nrow() / nrow(nhs.eth)

## subset societies with 10 or more songs
gjb.soc.10 <- gjb.soc %>% filter(n > 9 & !is.na(prop_group))
nhs.soc.10 <- nhs.soc %>% filter(n > 9 & !is.na(prop_group))

## percent societies with more group singing
# GJB 0.69 
gjb.soc.10 %>% filter(prop_group > 0.5) %>% nrow() / nrow(gjb.soc.10)
# NHS 0.73
nhs.soc.10 %>% filter(prop_group > 0.5) %>% nrow() / nrow(nhs.soc.10)


## mean group singing proportion
#GJB 0.64
mean(gjb.soc.10$prop_group, na.rm = T)

#NHS 0.66
mean(nhs.soc.10$prop_group, na.rm = T)

## societies without solo or group singing
# GJB
# without group 6%
gjb.soc.10 %>% filter(prop_group == 0) %>% nrow() / nrow(gjb.soc.10)
# without solo 19%
gjb.soc.10 %>% filter(prop_group == 1) %>% nrow() / nrow(gjb.soc.10)
# NHS
# without group 0
nhs.soc.10 %>% filter(prop_group == 0) %>% nrow() / nrow(nhs.soc.10)
# without solo 6%
nhs.soc.10 %>% filter(prop_group == 1) %>% nrow() / nrow(nhs.soc.10)

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
         EA202,
         EA031,
         std_EA031,
         tsd,
         ri) %>% 
  group_by(ea_id) %>% 
  summarise(across(where(is.double), mean, na.rm = T, .names = '{.col}'), n = sum(n, na.rm = T))

gjb.soc.ea <- gjb.soc %>% 
  filter(!is.na(ea_id)) %>% 
  select(ea_id, Region, xd_id) %>% 
  distinct(ea_id, .keep_all = T) %>% 
  left_join(gjb.soc.ea,.,by = 'ea_id')

## Multilevel models GJB + NHS ####

## combine datasets, merge societies in both
gjb.nhs.full <- gjb.soc.ea %>% 
  select(ea_id, n, prop_group, EA202, EA031, std_EA031, tsd, ri, Region) %>% 
  mutate(society_id = 'GJB') %>% 
  full_join(.,select(nhs.soc, id_nhs, ea_id, n, prop_group, EA202, EA031, std_EA031, tsd, ri, Region), by = 'ea_id') %>%
  rowwise() %>% 
  mutate(prop_group = mean(c(prop_group.x,prop_group.y), na.rm = T),
         EA031 = ifelse(!is.na(EA031.x), EA031.x, EA031.y),
         EA202 = ifelse(!is.na(EA202.x), EA202.x, EA202.y),
         std_EA031 = ifelse(!is.na(std_EA031.x), std_EA031.x, std_EA031.y),
         tsd = ifelse(!is.na(tsd.x), tsd.x, tsd.y),
         ri = ifelse(!is.na(ri.x), ri.x, ri.y),
         Region = ifelse(!is.na(Region.x), Region.x, Region.y),
         n = ifelse(!is.na(n.x) & !is.na(n.y), n.x + n.y,
                    ifelse(!is.na(n.x), n.x, n.y)),
         dataset = ifelse(!is.na(society_id)&!is.na(id_nhs), 'Both',
                          ifelse(!is.na(id_nhs), 'NHS','GJB'))) %>% 
  select(-contains('.'))

## Population size
d.ea202 <- gjb.nhs.full %>% 
  filter(n > 9 & !is.na(EA202))

m.ea202 <- lmer(prop_group ~ log(EA202) + factor(dataset) + (1|Region),
                data = d.ea202)

## Local community size
d.ea031 <- gjb.nhs.full %>% 
  filter(n > 9 & !is.na(std_EA031))

m.ea031 <- lmer(prop_group ~ poly(std_EA031,2) + factor(dataset) + (1|Region),
                data = d.ea031)

tab_model(m.ea202,m.ea031)

## Same models, GJB and NHS fitted separately
m.ea202.gjb <- lmer(prop_group ~ log(EA202) + (1|Region),
                    data = subset(gjb.soc.ea, n > 9))

m.ea031.gjb <- lmer(prop_group ~ poly(std_EA031,2) + (1|Region),
                    data = subset(gjb.soc.ea, n > 9 & !is.na(std_EA031)))

m.ea202.nhs <- lmer(prop_group ~ log(EA202) + (1|Region),
                    data = subset(nhs.soc, n > 9))

m.ea031.nhs <- lmer(prop_group ~ poly(std_EA031,2) + (1|Region),
                    data = subset(nhs.soc, n > 9 & !is.na(std_EA031)))

tab_model(m.ea202.gjb,m.ea031.gjb,m.ea202.nhs,m.ea031.nhs)

## Social complexity data
gjb.nhs.sccs <- gjb.nhs.full %>% filter(!is.nan(tsd) & n > 1)

# Social differentiation
m.tsd <- lmer(prop_group ~ tsd + factor(dataset) + (1|Region), gjb.nhs.sccs)

# Population size and agriculture
m.ri <- lmer(prop_group ~ ri + factor(dataset) + (1|Region), gjb.nhs.sccs)

tab_model(m.tsd,m.ri)

## Cumulative multilevel models ####

# data for models
d.gjb.sc <- gjb.soc %>%
  filter(n > 9) %>%
  select(society_id, xd_id, Region, tsd, ri) %>%
  left_join(select(gjb.line_1, song_id, society_id, group.score), ., by = 'society_id') %>%
  mutate(across(c(group.score), ordered)) 

# SCCS subset
d.sc.sccs <- d.gjb.sc %>% 
  select(song_id, xd_id, Region,group.score, tsd, ri) %>% 
  drop_na()

# Cumulative multilevel models
bm.sd.tsd9 <- brm(group.score ~ tsd + (1|xd_id) + (1|Region),
                 data = d.sc.sccs,
                 family = 'cumulative',
                 warmup = 1500,
                 iter = 5000,
                 cores = 4,
                 control = list(adapt_delta = 0.9))

bm.sd.ri9 <- brm(group.score ~ ri + (1|xd_id) + (1|Region),
                data = d.sc.sccs,
                family = 'cumulative',
                warmup = 1500,
                iter = 5000,
                cores = 4,
                control = list(adapt_delta = 0.9))

# mcmc diagnostics
mcmc_trace(bm.sd.tsd9, regex_pars = '^b_')
mcmc_rank_hist(bm.sd.tsd9, regex_pars = '^b_')
mcmc_trace(bm.sd.ri9, regex_pars = '^b_')
mcmc_rank_hist(bm.sd.ri9, regex_pars = '^b_')

# parameter estimates
summary(bm.sd.tsd9)
summary(bm.sd.ri9)

## Social context ####

# List of contexts used
contexts <- c('Religious practices',
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
                  'Religious practices (religious experiences, prayers, sacrifices, purification, divination)' = 'Religious practices',
                  'Death (burials, funerals, mourning)' = 'Mourning',
                  'Sickness, medical care, and shamans' = 'Healing',
                  'Infancy and childhood' = 'Infant care',
                  'Cultural identity and pride' = 'Ceremonies',
                  'Agriculture' = 'Work',
                  'Labor' = 'Work')) %>%
  left_join(select(nhs.eth.text, indx, kf_context), by = 'indx') %>% 
  mutate(context = replace(context, grepl('story',kf_context), 'Storytelling')) %>%
  filter(context %in% contexts) %>% 
  select(indx, id_nhs, context, group) %>% 
  left_join(select(nhs.soc, id_nhs, Region, tsd, ri), by = 'id_nhs')

# make religious practices the reference category
nhs.context$context <- factor(nhs.context$context)
nhs.context$context <- relevel(nhs.context$context, ref = 'Religious practices')

# Fit and compare intercept only and context models
# intercept only
m.context.intercept <- glmer(group ~ 1 + (1|id_nhs),
                             family = binomial,
                             nhs.context)

# with context
m.context <- glmer(group ~ context + (1|id_nhs),
                   family = binomial,
                   nhs.context)
     
# ΔAIC = 103
AIC(m.context.intercept) - AIC(m.context)

# data for comparing context and social complexity models
nhs.context.comp <- nhs.context %>% 
  select(indx,id_nhs,context,group,tsd,ri) %>% 
  drop_na()

# compare context and social complexity as predictors
m.context.comp <- glmer(group ~ context + (1|id_nhs),
                        family = binomial,
                        nhs.context.comp)
                         
m.context.tsd <- glmer(group ~ tsd + (1|id_nhs),
                       family = binomial,
                       nhs.context.comp) 

m.context.ri <- glmer(group ~ ri + (1|id_nhs),
                      family = binomial,
                      nhs.context.comp)

# ΔAIC = 86.8
AIC(m.context.tsd) - AIC(m.context.comp)                             

# ΔAIC = 86.6
AIC(m.context.ri) - AIC(m.context.comp)

# model with both context and social differentiation
m.tsd.context <- glmer(group ~ context + tsd + (1|id_nhs),
                       family = binomial,
                       nhs.context.comp)
                             
# ΔAIC = -0.04; social differentiation does not improve model
AIC(m.tsd.context) - AIC(m.context.comp)

## social complexity models for the same social context

## Religious practices
d.relig <- nhs.context %>% 
  filter(context == 'Religious practices' & !is.na(tsd))

m.relig1 <- glmer(group ~ tsd + (1|id_nhs),
                 family = binomial,
                 d.relig,
                 control = glmerControl(optimizer = 'bobyqa'))

m.relig2 <- glmer(group ~ ri + (1|id_nhs),
                 family = binomial,
                 d.relig,
                 control = glmerControl(optimizer = 'bobyqa'))

tab_model(m.relig1,m.relig2)

## Healing
d.heal <- nhs.context %>% 
  filter(context == 'Healing' & !is.na(tsd))

m.heal1 <- glmer(group ~ tsd + (1|id_nhs),
                 family = binomial,
                 d.heal,
                 control = glmerControl(optimizer = 'bobyqa'))

# singular fit
m.heal2 <- glmer(group ~ ri + (1|id_nhs),
                family = binomial,
                d.heal,
                control = glmerControl(optimizer = 'bobyqa'))

tab_model(m.heal1,m.heal2)

## GJB and NHS comparison ####

# create dataframe with societies in both GJB and NHS
gjb.nhs <- nhs.soc %>% 
  filter(ea_id != '.') %>% 
  select(id_nhs, society, ea_id, prop_group, n) %>% 
  left_join(.,select(gjb.soc.ea, ea_id, n, prop_group), by = 'ea_id') %>% 
  filter(!is.na(prop_group.x) & !is.na(prop_group.y)) %>% 
  distinct(ea_id, .keep_all = T) %>% 
  mutate(prop_diff = abs(prop_group.x - prop_group.y))

# median = 0.24
median(gjb.nhs$prop_diff)

## permutation test
permuteProps <- function(props){
  props$gjb <- sample(props$gjb)
  prop_diff <- median(abs(props$nhs - props$gjb))
  return(prop_diff)
}

# for all societies (n=42)
props <- gjb.nhs %>% 
  select(ea_id, prop_group.x, prop_group.y) %>% 
  rename(nhs = prop_group.x, gjb = prop_group.y)

true_diff <- median(abs(props$nhs - props$gjb))

diffs <- replicate(10000, permuteProps(props))

# z score
(true_diff - mean(diffs))/sd(diffs)

# p value
sum(diffs < true_diff)/10000


# for societies with sample 10 or more (n=17)
props.10 <- gjb.nhs %>% 
  filter(n.x > 9 & n.y > 9) %>% 
  select(ea_id, prop_group.x, prop_group.y) %>% 
  rename(nhs = prop_group.x, gjb = prop_group.y)

# median = 0.16
true_diff.10 <- median(abs(props.10$nhs - props.10$gjb))

diffs.10 <- replicate(10000, permuteProps(props.10))

# z score
(true_diff.10 - mean(diffs.10))/sd(diffs.10)

# p value
sum(diffs.10 < true_diff.10)/10000

## permutation test considering direction
permuteGroups <- function(props){
  diffs = apply(props[,c("nhs","gjb")],1,function(X){
    X = sample(X)
    X[1] - X[2]
  })
  prop_diff <- median(diffs)
  return(prop_diff)
}

# for all societies (n=42)
props <- gjb.nhs %>% 
  select(ea_id, prop_group.x, prop_group.y) %>% 
  rename(nhs = prop_group.x, gjb = prop_group.y)

true_diffdir <- median(props$nhs - props$gjb)

diffs <- replicate(10000, permuteGroups(props))

(true_diffdir - mean(diffs))/sd(diffs)

sum(diffs > true_diffdir)/10000

# for societies with sample 10 or more (n=17)
props.10 <- gjb.nhs %>% 
  filter(n.x > 9 & n.y > 9) %>% 
  select(ea_id, prop_group.x, prop_group.y) %>% 
  rename(nhs = prop_group.x, gjb = prop_group.y)

true_diffdir.10 <- median(props.10$nhs - props.10$gjb)

diffs <- replicate(10000, permuteGroups(props.10))

(true_diffdir.10 - mean(diffs))/sd(diffs)

sum(diffs > true_diffdir.10)/10000

