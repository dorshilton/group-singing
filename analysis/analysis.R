suppressPackageStartupMessages({
  library(tidyverse)
  library(lme4)
  library(mgcv)
  library(brms)
  library(bayesplot)
  library(bayestestR)
  library(patchwork)
  library(ggpubr)
  library(ggbeeswarm)
  library(ggridges)
})

## Read processed data ####
gjb.line_1 <- read.csv('data/processed/gjb_line_1.csv')
gjb.soc <- read.csv('data/processed/gjb_soc.csv')
gjb.soc.ea <- read.csv('data/processed/gjb_soc_ea.csv')
gjb.context <- read.csv('data/processed/gjb_context.csv')
nhs.eth <- read.csv('data/processed/nhs_eth.csv')
nhs.soc <- read.csv('data/processed/nhs_soc.csv')
nhs.context <- read.csv('data/processed/nhs_context.csv')
sccs.soc <- read.csv('data/processed/sccs_soc.csv')

## Descriptive stats ####

## overall predominance of group singing

# GJB 0.67
gjb.line_1 %>% 
  distinct(song_id, group, .keep_all = T) %>% 
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
# GJB 0.70 
gjb.soc.10 %>% filter(prop_group > 0.5) %>% nrow() / nrow(gjb.soc.10)
# NHS 0.73
nhs.soc.10 %>% filter(prop_group > 0.5) %>% nrow() / nrow(nhs.soc.10)


## mean group singing proportion
#GJB 0.66
mean(gjb.soc.10$prop_group, na.rm = T)

#NHS 0.66
mean(nhs.soc.10$prop_group, na.rm = T)

## societies without solo or group singing
# GJB
# without group 4%
gjb.soc.10 %>% filter(prop_group == 0) %>% nrow() / nrow(gjb.soc.10)
# without solo 22%
gjb.soc.10 %>% filter(prop_group == 1) %>% nrow() / nrow(gjb.soc.10)
# NHS
# without group 0
nhs.soc.10 %>% filter(prop_group == 0) %>% nrow() / nrow(nhs.soc.10)
# without solo 6%
nhs.soc.10 %>% filter(prop_group == 1) %>% nrow() / nrow(nhs.soc.10)

# create dataframe with societies in both GJB and NHS
gjb.nhs <- nhs.soc %>% 
  filter(ea_id != '.') %>% 
  select(id_nhs, society, ea_id, prop_group, n) %>% 
  left_join(.,select(gjb.soc.ea, ea_id, n, prop_group), by = 'ea_id') %>% 
  filter(!is.na(prop_group.x) & !is.na(prop_group.y)) %>% 
  distinct(ea_id, .keep_all = T) %>% 
  mutate(prop_diff = abs(prop_group.x - prop_group.y))

# median group singing proportion: 0.24
gjb.nhs %>%
  summarize(n = n(), median = median(prop_diff))

# median group singing proportion for societies with minimum sample of 10: 0.15
gjb.nhs %>% 
  filter(n.x > 9 & n.y > 9) %>% 
  summarize(n = n(), median = median(prop_diff))

## Figure 1 ####

## 1.1: Maps
world <- map_data('world')

# GJB
# combine merged EA societies with GJB samples not in EA
gjb.soc.no_ea <- gjb.soc %>%
  filter(is.na(ea_id) & n > 0) %>% 
  select(Society_latitude,
         Society_longitude,
         ea_id,
         n,
         n_ea,
         prop_group,
         EA031,
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
  geom_vline(xintercept = 0.5, linetype = 'longdash', linewidth = 1) +
  scale_size_continuous(range = c(2, 12), name = '') +
  scale_x_continuous(breaks = seq(0,1, by = 0.25),limits = c(0,1)) +
  scale_fill_manual(values = c('#B28AD6','#350054'), name = '') +
  labs(x = 'Proportion of Group singing',
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

## Social context ####

## NHS context frequencies
nhs.context %>% 
  group_by(context) %>% 
  summarise(n = n(), group_songs = sum(group)) %>% 
  mutate(prop_group = round(group_songs/n,2)) %>% 
  arrange(desc(prop_group), desc(n))

# Fit and compare intercept only and context models
nhs.context$context <- factor(nhs.context$context)

# intercept only
m.nhs.context.intercept <- glmer(group ~ 1 + (1|id_nhs),
                             family = binomial,
                             nhs.context)

# with context
m.nhs.context <- glmer(group ~ context + (1|id_nhs),
                   family = binomial,
                   nhs.context)

# ΔAIC = 103
AIC(m.nhs.context.intercept) - AIC(m.nhs.context)

# Conditional R2: 0.41
# Marginal R2: 0.16
performance::r2_nakagawa(m.nhs.context)

## GJB contexts

## Check automatic and manual coding inter-rater reliability

# read manually coded sample
coding.sample <- read.csv('data/raw/GJB/GJB_context_manual.csv')

# automatic coding
coding.sample$religious <- 0
coding.sample$religious[grep('religious|ritual|rite|pray|initiation|spirit|shaman',coding.sample$Song, ignore.case = T)] <- 1
coding.sample$religious[grep('religious|ritual|rite|pray|initiation|spirit|shaman',coding.sample$Genre, ignore.case = T)] <- 1
coding.sample$religious[grep('religious|ritual|rite|pray|initiation|spirit|shaman',coding.sample$Song_notes, ignore.case = T)] <- 1

coding.sample$dance <- 0
coding.sample$dance[grep('dance|dancing',coding.sample$Song, ignore.case = T)] <- 1
coding.sample$dance[grep('dance|dancing',coding.sample$Genre, ignore.case = T)] <- 1
coding.sample$dance[grep('dance|dancing',coding.sample$Song_notes, ignore.case = T)] <- 1

coding.sample$mourning <- 0
coding.sample$mourning[grep('mourn|lament|funera',coding.sample$Song, ignore.case = T)] <- 1
coding.sample$mourning[grep('mourn|lament|funera',coding.sample$Genre, ignore.case = T)] <- 1
coding.sample$mourning[grep('mourn|lament|funera',coding.sample$Song_notes, ignore.case = T)] <- 1

coding.sample$healing <- 0
coding.sample$healing[grep('heal|curing|cure',coding.sample$Song, ignore.case = T)] <- 1
coding.sample$healing[grep('heal|curing|cure',coding.sample$Genre, ignore.case = T)] <- 1
coding.sample$healing[grep('heal|curing|cure',coding.sample$Song_notes, ignore.case = T)] <- 1

coding.sample$lullaby <- 0
coding.sample$lullaby[grep('lullab',coding.sample$Song, ignore.case = T)] <- 1
coding.sample$lullaby[grep('lullab',coding.sample$Genre, ignore.case = T)] <- 1
coding.sample$lullaby[grep('lullab',coding.sample$Song_notes, ignore.case = T)] <- 1

# 0.64
psych::cohen.kappa(cbind(coding.sample$religious,coding.sample$religious_manual))

# 0.91
psych::cohen.kappa(cbind(coding.sample$dance,coding.sample$dance_manual))

# 0.49
psych::cohen.kappa(cbind(coding.sample$mourning,coding.sample$mourning_manual))

# 1
psych::cohen.kappa(cbind(coding.sample$healing,coding.sample$healing_manual))

# 1
psych::cohen.kappa(cbind(coding.sample$lullaby,coding.sample$lullaby_manual))

## context frequencies
gjb.context %>% 
  pivot_longer(cols = Religious:Lullaby,
               names_to = 'context',
               values_to = 'code') %>% 
  filter(code == 1) %>% 
  group_by(context) %>% 
  summarise(n = n(), group_songs = sum(group)) %>% 
  mutate(prop_group = round(group_songs/n,2)) %>% 
  arrange(desc(prop_group), desc(n))

# Fit and compare intercept only and context models
# intercept only
m.gjb.context.intercept <- glmer(group ~ 1 + (1|society_id),
                                 family = binomial,
                                 gjb.context)

# with context
m.gjb.context <- glmer(group ~ Religious + Dance + Mourning + Healing + Lullaby + (1|society_id),
                       family = binomial,
                       gjb.context)

# Conditional R2: 0.58
# Marginal R2: 0.12
performance::r2_nakagawa(m.gjb.context)

# ΔAIC = 152
AIC(m.gjb.context.intercept) - AIC(m.gjb.context)

## Figure 2 ####

# extract predictions
nhs.context.pred <- nhs.context %>%
  cbind(., predict(m.nhs.context, type = 'response')) %>% 
  rename(Estimate = `predict(m.nhs.context, type = "response")`)

# summarize frequencies
nhs.context.freq <- nhs.context %>% 
  group_by(context) %>% 
  summarise(soc = sum(n_distinct(id_nhs)), n = n()) %>%
  mutate(label = paste(context,'\n(',as.character(soc),' / ',as.character(n),')', sep = '')) %>% 
  arrange(n)

# plot model predictions (ridges)
nhs_context <- ggplot(nhs.context.pred, 
                aes(x = Estimate, y = context, fill = after_stat(x))) +
  geom_density_ridges_gradient(scale = 1.2, rel_min_height = 0.05, 
                                         alpha = 0.2,
                                         jittered_points = TRUE,
                                         position = position_raincloud(adjust_vlines = T)) +
  scale_fill_gradient2(low = '#357EBD',
                       mid = '#955bb7',
                       high = '#D43F3A',
                       midpoint = 0.5) +
  ggtitle('Natural History of Song') +
  labs(x = 'Probability of group singing', y = 'Social Context') +
  scale_x_continuous(breaks = c(0.0,0.25,0.5,0.75,1), limits = c(0,1)) +
  scale_y_discrete(limits = nhs.context.freq$context,
                   labels = nhs.context.freq$label) +
  theme_minimal() +
  theme(legend.position = 'none',
        axis.title.y = element_text(vjust = 2.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))

  
## GJB
gjb.context.pred <- gjb.context %>%
  cbind(., predict(m.gjb.context, type = 'response')) %>% 
  rename(Estimate = `predict(m.gjb.context, type = "response")`) %>% 
  pivot_longer(cols = Religious:Lullaby, names_to = 'context', values_to = 'code') %>% 
  filter(code == 1) %>% 
  select(-code)

# summarize frequencies
gjb.context.freq <- gjb.context %>% 
  select(song_id,society_id,Religious,Dance,Mourning,Healing,Lullaby) %>% 
  pivot_longer(cols = Religious:Lullaby,
               names_to = 'context',
               values_to = 'code') %>% 
  filter(code == 1) %>% 
  group_by(context) %>% 
  summarise(soc = sum(n_distinct(society_id)), n = n()) %>%
  mutate(label = paste(context,'\n(',as.character(soc),' / ',as.character(n),')', sep = '')) %>% 
  arrange(factor(context, levels = c('Lullaby','Healing','Mourning','Dance','Religious')))

# plot model predictions (ridges)
gjb_context <- ggplot(gjb.context.pred, 
                      aes(x = Estimate, y = context, fill = after_stat(x))) +
  ggridges::geom_density_ridges_gradient(scale = 1.2, rel_min_height = 0.05, 
                                         alpha = 0.2,
                                         jittered_points = TRUE,
                                         position = position_raincloud(adjust_vlines = T)) +
  scale_fill_gradient2(low = '#357EBD',
                       mid = '#955bb7',
                       high = '#D43F3A',
                       midpoint = 0.5) +
  ggtitle('Global Jukebox') +
  labs(x = 'Probability of group singing', y = 'Social Context') +
  scale_x_continuous(breaks = c(0.0,0.25,0.5,0.75,1), limits = c(0,1)) +
  scale_y_discrete(limits = gjb.context.freq$context,
                   labels = gjb.context.freq$label) +
  theme_minimal() +
  theme(legend.position = 'none',
        axis.title.y = element_text(vjust = 2.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))

fig2 <- wrap_plots(nhs_context, gjb_context,nrow = 2, heights = c(5,3)) + plot_annotation(tag_levels = 'A') &
  theme(plot.tag = element_text(face = 'bold'))

ggsave('results/Fig2.pdf', fig2, height = 10, width = 8)


## Community size ####

# confirm choice of quadratic regression using GAM
gam.ea031.gjb <- gam(formula = prop_group ~ s(std_EA031, k = 8),
                     data = subset(gjb.soc.ea, n > 9),
                     method = 'REML')

# edf = 2.64 p < 0.001
summary(gam.ea031.gjb)

## GJB
d.gjb.ea031 <- gjb.soc %>%
  filter(n_ea > 9) %>%
  select(society_id, ea_id, xd_id, std_EA031) %>%
  left_join(select(gjb.line_1, society_id, group), ., by = 'society_id') %>% 
  drop_na(std_EA031)

m.ea031.gjb <- glmer(group ~ poly(std_EA031, 2, raw = TRUE) + (1|ea_id),
                     family = binomial,
                     data = d.gjb.ea031)

summary(m.ea031.gjb)

performance::r2_nakagawa(m.ea031.gjb)

## NHS
d.nhs.ea031 <- nhs.soc %>%
  filter(n > 9) %>%
  select(id_nhs, ea_id, xd_id, std_EA031) %>%
  left_join(select(nhs.eth, id_nhs, group), ., by = 'id_nhs') %>% 
  drop_na(std_EA031)

m.ea031.nhs <- glmer(group ~ poly(std_EA031, 2, raw = TRUE) + (1|ea_id),
                     family = binomial,
                     data = d.nhs.ea031)

summary(m.ea031.nhs)

performance::r2_nakagawa(m.ea031.nhs)

## Both

# combine datasets
d.gjb.ea031 <- d.gjb.ea031 %>% mutate(dataset = 'GJB')
d.nhs.ea031 <- d.nhs.ea031 %>% mutate(dataset = 'NHS') %>% rename(society_id = id_nhs)
d.both.ea031 <- rbind(d.gjb.ea031, d.nhs.ea031)

m.ea031.both <- glmer(group ~ poly(std_EA031, 2, raw = TRUE) + (1|ea_id),
                      family = binomial,
                      data = d.both.ea031)

summary(m.ea031.both)

performance::r2_nakagawa(m.ea031.both)

# include only societies in SCCS

d.ea031.sccs <- d.both.ea031 %>% filter(!is.na(xd_id))

m.ea031.sccs <- glmer(group ~ poly(std_EA031, 2, raw = TRUE) + (1|ea_id),
                      family = binomial,
                      data = d.ea031.sccs)

summary(m.ea031.sccs)

performance::r2_nakagawa(m.ea031.sccs)

## Figure 3 ####

## combine datasets, merge societies in both
gjb.nhs.full <- gjb.soc.ea %>% 
  select(ea_id, n, prop_group, EA031, std_EA031, tsd, ri, Region) %>% 
  mutate(society_id = 'GJB') %>% 
  full_join(.,select(nhs.soc, id_nhs, ea_id, n, prop_group, EA031, std_EA031, tsd, ri, Region), by = 'ea_id') %>%
  rowwise() %>% 
  mutate(prop_group = mean(c(prop_group.x,prop_group.y), na.rm = T),
         EA031 = ifelse(!is.na(EA031.x), EA031.x, EA031.y),
         std_EA031 = ifelse(!is.na(std_EA031.x), std_EA031.x, std_EA031.y),
         tsd = ifelse(!is.na(tsd.x), tsd.x, tsd.y),
         ri = ifelse(!is.na(ri.x), ri.x, ri.y),
         Region = ifelse(!is.na(Region.x), Region.x, Region.y),
         n = ifelse(!is.na(n.x) & !is.na(n.y), n.x + n.y,
                    ifelse(!is.na(n.x), n.x, n.y)),
         dataset = ifelse(!is.na(society_id)&!is.na(id_nhs), 'Both',
                          ifelse(!is.na(id_nhs), 'NHS','GJB'))) %>% 
  select(-contains('.'))

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
  geom_beeswarm(aes(color = prop_group, size = n), alpha=0.6) +
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
  ggtitle('Global Jukebox + Natural History of Song') +
  theme_minimal() +
  theme(legend.position = 'none', 
        plot.title = element_text(hjust = 0.5),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 10))

gjb_31 <- ggplot(subset(gjb.soc.ea, n > 9 & !is.na(EA031)), 
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
  geom_beeswarm(aes(color = prop_group, size = n), alpha=0.6) +
  geom_smooth(aes(x = EA031, y = prop_group),
              method = 'lm',
              formula = y ~ splines::bs(x, 3),
              se = T,
              color = 'black',
              alpha = 0.2) +
  scale_y_continuous(limits = c(0,1)) +
  scale_size_continuous(range = c(2, 4)) +
  scale_color_gradient2(low = '#357EBD',
                        mid = '#955bb7',
                        high = '#D43F3A',
                        midpoint = 0.5) +
  geom_hline(yintercept = 0.5, linetype = 2) +
  labs(x = 'Local Community Size', y = 'Proportion of Group Singing') +
  ggtitle('Global Jukebox') +
  theme_minimal() +
  theme(legend.position = 'none', plot.title = element_text(hjust = 0.5))

nhs_31 <- ggplot(subset(nhs.soc, n > 9 & !is.na(EA031)), 
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
  geom_beeswarm(aes(color = prop_group, size = n), alpha=0.6) +
  geom_smooth(aes(x = EA031, y = prop_group),
              method = 'lm',
              formula = y ~ x,
              se = T,
              color = 'black',
              alpha = 0.2) +
  scale_y_continuous(limits = c(0,1)) +
  scale_size_continuous(range = c(2, 4)) +
  scale_color_gradient2(low = '#357EBD',
                        mid = '#955bb7',
                        high = '#D43F3A',
                        midpoint = 0.5) +
  geom_hline(yintercept = 0.5, linetype = 2) +
  labs(x = 'Local Community Size', y = 'Proportion of Group Singing') +
  ggtitle('Natural History of Song') +
  theme_minimal() +
  theme(legend.position = 'none', plot.title = element_text(hjust = 0.5))


design2 = c('AAAABBBB
            ##CCCC##')

fig3 <- wrap_plots(gjb_31, nhs_31, both_031, nrow = 2, design = design2) + plot_annotation(tag_levels = 'A')

ggsave('results/Fig3.pdf', fig3, height = 8, width = 10)


## Social Differentiation ####

## proportion of group singing 

## GJB
d.gjb.tsd <- gjb.soc %>%
  filter(n > 9) %>%
  select(society_id, xd_id, tsd) %>%
  left_join(select(gjb.line_1, society_id, group), ., by = 'society_id') %>% 
  drop_na(tsd)

m.tsd.gjb <- glmer(group ~ tsd + (1|xd_id),
                     family = binomial,
                     data = d.gjb.tsd)

summary(m.tsd.gjb)

## NHS
d.nhs.tsd <- nhs.soc %>%
  filter(n > 9) %>%
  select(id_nhs, xd_id, tsd) %>%
  left_join(select(nhs.eth, id_nhs, group), ., by = 'id_nhs') %>% 
  drop_na(tsd)

m.tsd.nhs <- glmer(group ~ tsd + (1|xd_id),
                     family = binomial,
                     data = d.nhs.tsd)

summary(m.tsd.nhs)


## Sociovocal scale

# data for models
d.gjb.sc <- gjb.soc %>%
  filter(n > 9) %>%
  select(society_id, xd_id, tsd, ri) %>%
  left_join(select(gjb.line_1, song_id, society_id, group.score), ., by = 'society_id') %>%
  mutate(across(c(group.score), ordered)) 

# SCCS subset
d.sc.sccs <- d.gjb.sc %>% 
  select(song_id, xd_id, group.score, tsd, ri) %>% 
  drop_na()

# Cumulative multilevel models
bm.sv.tsd <- brm(group.score ~ tsd + (1|xd_id),
                 data = d.sc.sccs,
                 family = 'cumulative',
                 warmup = 1500,
                 iter = 5000,
                 cores = 4,
                 control = list(adapt_delta = 0.9))

bm.sv.ri <- brm(group.score ~ ri + (1|xd_id),
                data = d.sc.sccs,
                family = 'cumulative',
                warmup = 1500,
                iter = 5000,
                cores = 4,
                control = list(adapt_delta = 0.9))

# mcmc diagnostics
mcmc_trace(bm.sv.tsd, regex_pars = '^b_')
mcmc_rank_hist(bm.sv.tsd, regex_pars = '^b_')
mcmc_trace(bm.sv.ri, regex_pars = '^b_')
mcmc_rank_hist(bm.sv.ri, regex_pars = '^b_')

# parameter estimates
summary(bm.sv.tsd)
summary(bm.sv.ri)

# category-specific effects
bm.tsd.cs <- brm(group.score ~ cs(tsd) + (1|xd_id),
                 data = d.sc.sccs,
                 family = 'acat',
                 warmup = 1500,
                 iter = 5000,
                 cores = 4,
                 control = list(adapt_delta = 0.9))

summary(bm.tsd.cs)

## figure 4 #####

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


sv.tsd <- cbind(d.sc.sccs, predict(bm.sv.tsd))

d.fig4 <- sv.tsd %>% 
  rename(Interlocked = `P(Y = 1)`) %>% 
  rename(Unison = `P(Y = 2)`) %>% 
  rename(GroupGroup = `P(Y = 3)`) %>% 
  rename(SoloGroup = `P(Y = 4)`) %>% 
  rename(Solo = `P(Y = 5)`)

d.fig4 <- reshape2::melt(d.fig4, 
                         id.vars = c('song_id','xd_id','group.score','tsd','ri'),
                         variable.name = 'Level', value.name = 'Probability')

d.fig4$Level <- factor(d.fig4$Level, levels = c('Interlocked',
                                                'Unison',
                                                'GroupGroup',
                                                'SoloGroup',
                                                'Solo'))

# social differentiation model predictions
fig4 <- ggplot(d.fig4, aes(x = tsd, y = Probability, colour = Level, fill = Level)) +
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

ggsave('results/Fig4.pdf', fig4, width = 14, height = 4)

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

ggsave('results/FigS1.pdf', figS1)

## Figure S2: bar chart of singers_n values
figS2 <- ggplot(nhs.eth, aes(x = singers_n)) +
  geom_bar(stat = 'count') +
  labs(x = '') +
  theme_minimal() +
  coord_flip()

ggsave('results/FigS2.pdf', figS2)

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
  geom_vline(xintercept = 0.5, linetype = 'longdash', linewidth = 1) +
  scale_size_continuous(range = c(2, 5)) +
  labs(x = 'Group singing',
       y = '') +
  theme_minimal() +
  theme(legend.position = 'none') +
  theme(axis.title = element_text(size = 13),
        axis.text = element_text(size = 10))

ggsave('results/FigS3.pdf', figS3)

## control test

# check group singing predominance with songs coded both solo and group counted as solo in GJB

# percent of songs overall 0.65 (instead of 0.67)
gjb.line_1 %>% 
  distinct(song_id, .keep_all = T) %>% 
  filter(group == 1) %>% 
  nrow() / nrow(distinct(gjb.line_1, song_id))

# percent societies with more group singing 0.69  (instead of 0.70)
gjb.soc <- gjb.line_1 %>%
  distinct(song_id, .keep_all = T) %>%
  group_by(society_id) %>%
  summarise(control_group_songs = sum(group)) %>%
  left_join(gjb.soc, ., by = 'society_id')

gjb.soc <- gjb.soc %>% 
  mutate(control_prop_group = control_group_songs/n)

gjb.soc.10 <- gjb.soc %>% filter(n > 9 & !is.na(prop_group))

gjb.soc.10 %>% filter(control_prop_group > 0.5) %>% nrow() / nrow(gjb.soc.10)

# mean proportion of group singing per society 0.64 (instead of 0.66)
mean(gjb.soc.10$control_prop_group, na.rm = T)

## Permutation tests ####

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

true_diff.10 <- median(abs(props.10$nhs - props.10$gjb))

diffs.10 <- replicate(10000, permuteProps(props.10))

# z score
(true_diff.10 - mean(diffs.10))/sd(diffs.10)

# p value
sum(diffs.10 < true_diff.10)/10000
