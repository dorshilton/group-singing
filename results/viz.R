suppressPackageStartupMessages({
  library(tidyverse)
  library(patchwork)
  library(ggpubr)
  library(ggbeeswarm)
  library(ggridges)
})
  
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



# NHS
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

wrap_plots(gjb_map,nhs_map,regions,ncol = 2, design = design, heights = c(3,3)) + plot_annotation(tag_levels = 'A') &
  theme(plot.tag = element_text(face = 'bold'))

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

wrap_plots(both_202, both_031, nrow = 1) + plot_annotation(tag_levels = 'A')

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
ggplot(d.fig3, aes(x = tsd, y = Probability, colour = Level, fill = Level)) +
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
ggplot(nhs.context.pred, 
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

## Supplementary Materials ####

## Figure S1: bar chart of line 1 values
ggplot(gjb.line_1, aes(x = factor(code,
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
ggplot(nhs.eth, aes(x = singers_n)) +
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

ggplot(gjb.nhs, aes(y = reorder(society, -prop_diff))) +
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
