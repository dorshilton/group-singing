suppressPackageStartupMessages({
  library(tidyverse)
  library(lme4)
  library(lavaan)
  library(semTools)
  library(brms)
  library(bayesplot)
  library(sjPlot)
  library(patchwork)
  library(ggpubr)
  library(ggbeeswarm)
  library(ggridges)
})

## Read processed data ####
gjb.line_1 <- read.csv('data/processed/gjb_line_1.csv')
gjb.soc <- read.csv('data/processed/gjb_soc.csv')
nhs.eth <- read.csv('data/processed/nhs_eth.csv')
nhs.eth.text <- read.csv('data/processed/nhs_eth_text.csv')
nhs.soc <- read.csv('data/processed/nhs_soc.csv')
sccs.soc <- read.csv('data/processed/sccs_soc.csv')

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
         std_EA031,
         tsd,
         ri) %>% 
  group_by(ea_id) %>% 
  summarise(across(where(is.double), mean, na.rm = T, .names = '{.col}'), n = sum(n, na.rm = T))

gjb.soc.ea <- gjb.soc %>% 
  filter(!is.na(ea_id)) %>% 
  select(ea_id, Region, xd_id, EA031) %>% 
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

## Figure 1 ####

## 1.1: Maps
world <- map_data('world')

# GJB
# create data with both merged EA societies and other samples
gjb.soc.no_ea <- gjb.soc %>%
  filter(is.na(ea_id) & n > 0) %>% 
  select(Society_latitude,
         Society_longitude,
         ea_id,
         n,
         prop_group,
         EA031,
         EA202,
         std_EA031,
         tsd,
         ri,
         Region,
         xd_id)

gjb.soc.map <- rbind(gjb.soc.ea, gjb.soc.no_ea)

gjb_map <- ggplot() + 
  geom_polygon(data = world, aes(x = long, y = lat, group = group), 
               fill = "#e6e3ea") + 
  coord_fixed(1.3, ylim = c(-55, 80), xlim = c(-165,180)) +
  geom_point(data = subset(gjb.soc.map, n > 9), 
             aes(x = Society_longitude, y = Society_latitude, size = n, fill = prop_group),
             alpha = 0.7,
             shape = 21) +
  scale_size_continuous(range = c(2, 5), 
                        name = 'Sample size') +
  scale_fill_gradient2(low = '#357EBD',
                       mid = '#955bb7',
                       high = '#D43F3A',
                       midpoint = 0.5,
                       name = 'Group singing') +
  ggtitle('Global Jukebox') +
  guides(size = guide_legend(override.aes = list(fill = '#955bb7')), 
         fill = guide_colorbar(title.vjust = 0.75)) +
  ggthemes::theme_map() +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'),
        legend.position = 'bottom',
        legend.justification = 'center')

nhs_map <- ggplot() + 
  geom_polygon(data = world, aes(x = long, y = lat, group = group), 
               fill = "#e6e3ea") + 
  coord_fixed(1.3, ylim = c(-55, 80), xlim = c(-165,180)) +
  geom_point(data = subset(nhs.soc, n > 9),
             aes(x = longitude, y = latitude, size = n, fill = prop_group),
             alpha = 0.7,
             shape = 21) +
  scale_size_continuous(range = c(2, 7),
                        name = 'Sample size') +
  scale_fill_gradient2(low = '#357EBD',
                       mid = '#955bb7',
                       high = '#D43F3A',
                       midpoint = 0.5,
                       name = 'Group singing') +
  ggtitle('Natural History of Song') + 
  guides(size = guide_legend(override.aes = list(fill = '#955bb7')), 
         fill = guide_colorbar(title.vjust = 0.75)) +
  ggthemes::theme_map() +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'),
        legend.position = 'bottom',
        legend.justification = 'center')

## 1.2: Group singing per region 
nhs.region <- nhs.soc %>%
  filter(Region != 'Western Asia') %>%
  group_by(Region) %>% 
  summarise(sample = sum(n, na.rm = T), group = sum(group_txt, na.rm = T)) %>% 
  mutate(prop_group = group/sample, dataset = 'Natural History of Song')

gjb.region <- gjb.soc %>%
  mutate(Region = replace(Region, Region == 'Australia', 'Oceania')) %>% 
  mutate(Region = replace(Region, grepl('Caribbean',Division), 'Caribbean')) %>% 
  group_by(Region) %>% 
  summarise(sample = sum(n, na.rm = T), group = sum(group_songs, na.rm = T)) %>% 
  mutate(prop_group = group/sample, dataset = 'Global Jukebox')

both.region <- rbind(gjb.region,nhs.region)

both.region <- both.region %>% 
  group_by(Region) %>% 
  summarise(GJB = sum(sample[dataset == 'Global Jukebox']),
            NHS = sum(sample[dataset == 'Natural History of Song'])) %>% 
  mutate(label = paste(Region,'\n(',as.character(GJB),',',as.character(NHS),')', sep = '')) %>%
  select(Region,label) %>% 
  left_join(both.region,.,by = 'Region')

regions <- ggplot(both.region, aes(x = prop_group, y = reorder(label, prop_group))) +
  geom_line(aes(group = label), color='grey', linetype = 'dashed') +
  geom_point(aes(fill = dataset, size = sample), color = 'black', shape = 21, alpha = 0.8) +
  geom_vline(xintercept = 0.5, linetype = 'longdash', size = 1) +
  scale_size_continuous(range = c(2, 12), name = '') +
  scale_x_continuous(breaks = seq(0,1, by = 0.25),limits = c(0,1)) +
  scale_fill_manual(values = c('#B28AD6','#350054'), name = '') +
  labs(x = 'Group singing',
       y = '') +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = 'bottom') +
  guides(size = 'none',
         fill = guide_legend(override.aes = list(size = 8)))

design = c('AAAAAAACCCC
            AAAAAAACCCC
            AAAAAAACCCC
            BBBBBBBCCCC
            BBBBBBBCCCC
            BBBBBBBCCCC')

fig1 <- wrap_plots(gjb_map,nhs_map,regions,ncol = 2, design = design, heights = c(3,3)) + plot_annotation(tag_levels = 'A') &
  theme(plot.tag = element_text(face = 'bold'))

ggsave('results/Fig1.pdf', fig1, width = 10)

## Figure 2 ####

both_202 <- ggplot(subset(gjb.nhs.full, n > 9 & !is.na(log(EA202))), 
                   aes(x = log(EA202),
                       y = prop_group)) +
  geom_point(aes(color = prop_group, size = n), alpha=0.6) +
  geom_smooth(aes(x = log(EA202), y = prop_group),
              method = 'lm',
              formula = y ~ x,
              se = T,
              color = 'black',
              alpha = 0.2) +
  scale_size_continuous(range = c(2, 4)) +
  scale_color_gradient2(low = '#357EBD',
                        mid = '#955bb7',
                        high = '#D43F3A',
                        midpoint = 0.5) +
  geom_hline(yintercept = 0.5, linetype = 2) +
  labs(x = 'Population Size (log)', y = 'Proportion of Group Singing') +
  theme_minimal() +
  theme(legend.position = 'none', 
        plot.title = element_text(hjust = 0.5),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 10))

both_031 <- ggplot(subset(gjb.nhs.full, n > 9 & !is.na(EA031)), 
                   aes(x = factor(EA031,
                                  labels = c("<50",
                                             "50-99",
                                             "100-199",
                                             "200-399",
                                             "400-1K",
                                             "1K+",
                                             "5K-50K",
                                             "50K+")),
                       y = prop_group)) +
  ggbeeswarm::geom_beeswarm(aes(color = prop_group, size = n), alpha=0.6) +
  geom_smooth(aes(x = EA031, y = prop_group),
              method = 'lm',
              formula = y ~ splines::bs(x, 3),
              se = T,
              color = 'black',
              alpha = 0.2) +
  scale_size_continuous(range = c(2, 4)) +
  scale_color_gradient2(low = '#357EBD',
                        mid = '#955bb7',
                        high = '#D43F3A',
                        midpoint = 0.5) +
  geom_hline(yintercept = 0.5, linetype = 2) +
  labs(x = 'Local Community Size', y = 'Proportion of Group Singing') +
  theme_minimal() +
  theme(legend.position = 'none', 
        plot.title = element_text(hjust = 0.5),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 10))

fig2 <- wrap_plots(both_202, both_031, nrow = 1) + plot_annotation(tag_levels = 'A')

ggsave('results/Fig2.pdf', fig2, width = 14)

## figure 3 #####

sociovocal_facet <- c(
  'Solo' = 'Solo',
  'SoloGroup' = 'Solo-Group',
  'GroupGroup' = 'Group-Group',
  'Unison' = 'Unison',
  'Interlocked' = 'Interlocked'
)

svcol <- c('#d43f3a',
           '#d13e76',
           '#af57a6',
           '#776fbf',
           '#357ebd')


sv.tsd <- cbind(d.sc.sccs, predict(bm.sd.tsd9))

d.fig3 <- sv.tsd %>% 
  rename(Solo = `P(Y = 1)`) %>% 
  rename(SoloGroup = `P(Y = 2)`) %>% 
  rename(GroupGroup = `P(Y = 3)`) %>% 
  rename(Unison = `P(Y = 4)`) %>% 
  rename(Interlocked = `P(Y = 5)`)

d.fig3 <- reshape2::melt(d.fig3, 
                         id.vars = c('song_id','xd_id','Region','group.score','tsd','ri'),
                         variable.name = 'Level', value.name = 'Probability')

d.fig3$Level <- factor(d.fig3$Level, levels = c('Interlocked',
                                                'Unison',
                                                'GroupGroup',
                                                'SoloGroup',
                                                'Solo'))

# social differentiation model predictions
fig3 <- ggplot(d.fig3, aes(x = tsd, y = Probability, colour = Level, fill = Level)) +
  geom_jitter(alpha = 0.3, size = 1, width = 0.02, height = 0.02) +
  geom_smooth(method = 'loess', alpha = 0.3, formula = y ~ x) +
  facet_wrap(~Level, labeller = labeller(Level = sociovocal_facet), nrow = 1) +
  labs(x = 'Social Differentiation', y = 'Probability') +
  scale_color_manual(values = svcol, name = 'Sociovocal organization') +
  scale_fill_manual(values = svcol, name = 'Sociovocal organization') +
  scale_x_continuous(breaks = c(-1,0,1)) +
  theme_minimal() + 
  theme(legend.position = 'none',
        strip.text.x = element_text(size = 12, face= 'italic'),
        panel.spacing = unit(1, "lines"),
        axis.title.x = element_text(vjust = -1.5),
        axis.title.y = element_text(vjust = 2.5),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 10))

ggsave('results/Fig3.pdf', fig3, width = 14, height = 4)

## Figure 4 ####

# extract predictions
nhs.context.pred <- nhs.context %>%
  cbind(., predict(m.context, type = 'response')) %>% 
  rename(Estimate = `predict(m.context, type = "response")`)

# summarize frequencies
context.freq <- nhs.context %>% 
  group_by(context) %>% 
  summarise(soc = sum(n_distinct(id_nhs)), n = n(), group_songs = sum(group)) %>% 
  mutate(prop = group_songs/n) %>% 
  mutate(prop = round(prop,2)) %>% 
  mutate(label = paste(context,'\n(',as.character(soc),',',as.character(n),')', sep = '')) %>% 
  arrange(n)

# plot model predictions (ridges)
fig4 <- ggplot(nhs.context.pred, 
       aes(x = Estimate, y = context, fill = stat(x))) +
  ggridges::geom_density_ridges_gradient(scale = 1.2, rel_min_height = 0.05, 
                                         alpha = 0.2,
                                         jittered_points = TRUE,
                                         position = position_raincloud(adjust_vlines = T)) +
  scale_fill_gradient2(low = '#357EBD',
                       mid = '#955bb7',
                       high = '#D43F3A',
                       midpoint = 0.5) +
  labs(x = 'Probability of group singing', y = 'Social Context') +
  scale_x_continuous(breaks = c(0.0,0.25,0.5,0.75,1), limits = c(0,1)) +
  scale_y_discrete(limits = context.freq$context,
                   labels = context.freq$label) +
  theme_minimal() +
  theme(legend.position = 'none',
        axis.title.y = element_text(vjust = 2.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))

ggsave('results/Fig4.pdf', fig4, width = 10)

## Supplementary Materials ####

## Figure S1: bar chart of line 1 values
figS1 <- ggplot(gjb.line_1, aes(x = factor(code,
                                  labels = c('2 - Solo',
                                             '4 - Alternating soloists',
                                             '5 - Unison (leader dominant)',
                                             '6 - Unison (group dominant)',
                                             '7 - Diffuse/individualized group',
                                             '8 - Alteration: leader-group',
                                             '9 - Alteration: group-group',
                                             '10 - Overlap: leader-group',
                                             '11 - Overlap: group-leader',
                                             '12 - Overlap: group-group',
                                             '13 - Interlock')))) + 
  geom_bar(stat = 'count') +
  labs(x = '') +
  theme_minimal() +
  coord_flip()

## Figure S2: bar chart of singers_n values
figS2 <- ggplot(nhs.eth, aes(x = singers_n)) +
  geom_bar(stat = 'count') +
  labs(x = '') +
  theme_minimal() +
  coord_flip()

## Figure S3: differences in group singing measures between GJB and NHS 
gjb.nhs <- nhs.soc %>% 
  filter(ea_id != '.') %>% 
  select(id_nhs, society, ea_id, prop_group, n) %>% 
  left_join(.,select(gjb.soc.ea, ea_id, n, prop_group), by = 'ea_id') %>% 
  filter(!is.na(prop_group.x) & !is.na(prop_group.y)) %>% 
  distinct(ea_id, .keep_all = T) %>% 
  mutate(prop_diff = abs(prop_group.x - prop_group.y))

figS3 <- ggplot(gjb.nhs, aes(y = reorder(society, -prop_diff))) +
  geom_point(aes(x = prop_diff), alpha = 0.7) +
  geom_point(aes(x = prop_group.x, size = n.x), color = '#08519c', alpha = 0.3) +
  geom_point(aes(x = prop_group.y, size = n.y), color = '#bc343d', alpha = 0.3) +
  geom_vline(xintercept = 0.5, linetype = 'longdash', size = 1) +
  scale_size_continuous(range = c(2, 5)) +
  labs(x = 'Group singing',
       y = '') +
  theme_minimal() +
  theme(legend.position = 'none') +
  theme(axis.title = element_text(size = 13),
        axis.text = element_text(size = 10))


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
  diffs <- apply(props[,c("nhs","gjb")],1,function(X){
    X <- sample(X)
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

