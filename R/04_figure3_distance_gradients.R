#### 04 - Figure 3 + Figure S1: buffer-distance land cover and change gradients
#
# Run from the repo root: Rscript R/04_figure3_distance_gradients.R
# Depends on: R/02_table1_accounting.R (sourced below, which itself sources
# R/00_setup.R) for clc_change_country_trans_netChng, used in the Figure S1
# "share of total net loss" panel.
# Writes: <OUTPUTS_DIR>/fig3.png, <OUTPUTS_DIR>/baselineLCdistFig.png,
#         <OUTPUTS_DIR>/shareChangeDistFig.png (both feed Figure S1)
#
# Author: Zander Venter

source("R/02_table1_accounting.R")

#### Figure 3a: land cover area share of buffer zone -------------------------
clc_change_totals <- clc_change %>%
  mutate(stratum = clc_class) %>%
  # Terrestrial surfaces
  filter(!stratum %in% c('Marine inlets', 'Rivers and lakes') ) %>%
  # group natural land cover
  mutate(stratum = if_else(!stratum %in% c('Urban', 'Cropland') & !str_detect(stratum, 'Gain|Loss'), 'Nature', stratum)) %>%
  group_by(stratum) %>%
  summarise(totArea = sum(area, na.rm=T)/1000000)

clc_dist_totals <- clc_change_distance %>%
  group_by(distance, type) %>%
  summarise(totArea = sum(area, na.rm=T)/1000000) %>%
  # Calculate band areas
  arrange(type, distance) %>%
  group_by(type) %>%
  mutate(totAreaBand = totArea - lag(totArea,1)) %>%
  mutate(totAreaBand = ifelse(is.na(totAreaBand), totArea, totAreaBand))

# Share of total landscape area within buffer distance
sp1 <- clc_change_distance%>%
  group_by(distance, stratum, type) %>%
  summarise(area = sum(area)/1000000)  %>%
  left_join(clc_dist_totals) %>%
  mutate(areaPerc = area/totArea*100) %>%
  mutate(clc_stablechange = ifelse(str_detect(stratum, ' '), 'change', 'stable')) %>%
  filter(clc_stablechange == 'stable') %>%
  ggplot(aes(x=distance, y = areaPerc, color=type)) +
  geom_point(alpha=0.25) +
  geom_line() +
  scale_x_continuous(breaks=c(2,4,6,8,10), labels = paste('<', c(2,4,6,8,10))) +
  labs(title = 'a) land cover area share of buffer zone',
       color='Buffer type',
       x = "Buffer distance from water's edge (km)",
       y = "Percentage of land area \nwithin buffer (%)") +
  facet_wrap(~ stratum, scales='free_y', nrow=1) +
  scale_color_manual(values = c('#3c0f73',  '#62c8ca','#4582bb'))+
  theme(legend.position='none',
        plot.title = element_text(size=11))

#### Figure 3b: habitat change intensity by buffer distance -------------------
# Get baseline nature areas
clc_natural_dist_totals <- clc_change_distance %>%
  # Nature land cover, including habitats lost since 2000 (i.e. they were
  # nature in 2000)
  filter(stratum %in% c('nature') | str_detect(stratum,  'nature ->')) %>%
  group_by(distance, type) %>%
  summarise(totAreaNature = sum(area, na.rm=T)/1000000) %>%
  arrange(type, distance) %>%
  group_by(type) %>%
  mutate(totAreaNatureBand = totAreaNature - lag(totAreaNature,1)) %>%
  mutate(totAreaNatureBand = ifelse(is.na(totAreaNatureBand), totAreaNature, totAreaNatureBand))

clc_netLoss_intensity_distance <-  clc_change_distance%>%
  filter(str_detect(stratum, '->')) %>%
  group_by(distance, stratum, type) %>%
  summarise(area = sum(area)/1000000) %>%
  mutate(clc_base = ifelse(str_detect(stratum, 'cropland'), 'cropland', 'urban'))%>%
  mutate(clc_lossgain = ifelse(str_detect(stratum, 'nature ->'), 'Loss', 'Gain'))%>%
  ungroup() %>%
  dplyr::select(distance,type,  clc_lossgain, clc_base, area) %>%
  pivot_wider(names_from=clc_lossgain, values_from=area)%>%
  # account for NA gain values (e.g. Tundra ecoregion has none)
  mutate(Gain = ifelse(is.na(Gain), 0, Gain)) %>%
  mutate(netLoss = Loss - Gain,
         recoverPerc = Gain / Loss * 100)  %>%
  left_join(clc_natural_dist_totals) %>%
  mutate(netLossIntensity = netLoss/totAreaNature*100) %>%
  mutate(stratum = ifelse(clc_base == 'urban', '(nature -> urban) - (urban -> nature)','(nature -> cropland) - (cropland -> nature)' )) %>%
  dplyr::select(distance, stratum, type, areaIntensity = netLossIntensity, netLoss)

sp2 <- clc_change_distance%>%
  filter(str_detect(stratum, '>')) %>%
  group_by(distance, stratum, type) %>%
  summarise(area = sum(area)/1000000)  %>%
  left_join(clc_natural_dist_totals) %>%
  mutate(areaIntensity = area/totAreaNature*100) %>%
  bind_rows(clc_netLoss_intensity_distance) %>%
  mutate(stratum = factor(stratum, levels = c("urban -> nature",
                                              "cropland -> nature",
                                              "nature -> urban",
                                              "nature -> cropland",
                                              "(nature -> urban) - (urban -> nature)",
                                              "(nature -> cropland) - (cropland -> nature)"))) %>%
  filter(str_detect(stratum, ' - ')) %>%
  ggplot(aes(x=distance, y = areaIntensity, color=type)) +
  geom_point(alpha=0.25) +
  geom_line()+
  scale_x_continuous(breaks=c(2,4,6,8,10), labels = paste('<', c(2,4,6,8,10))) +
  labs(title = 'b) habitat change share of baseline nature area within buffer zone',
       color='Buffer type',
       x = "Buffer distance from water's edge (km)",
       y = "Percentage of baseline nature\n area within buffer (%)") +
  facet_wrap(~ stratum, scales='free_y', nrow=1,  labeller = label_wrap_gen(width = 24)) +
  scale_color_manual(values = c('#3c0f73',  '#62c8ca','#4582bb'))+
  theme(plot.title = element_text(size=11))

fig3 <- grid.arrange(sp1,sp2, ncol=2, widths=c(1.1,1), padding = unit(0, "line"), newpage = F)
ggsave(out_path("fig3.png"), fig3, width = 35, height=10, units='cm')

#### Figure S1a: baseline land cover area share by distance band --------------
clc_baseline_distance <- clc_change_distance %>%
  mutate(stratum = as.character(stratum)) %>%
  mutate(stratum = ifelse(stratum == 'nature -> urban', 'nature',
                          ifelse(stratum == 'cropland -> nature', 'cropland',
                                 ifelse(stratum == 'nature -> cropland', 'nature',
                                        ifelse(stratum == 'urban -> nature', 'urban', stratum))))) %>%
  group_by(type, stratum, distance) %>%
  summarise(area = sum(area)/1000000) %>%
  mutate(areaPerc = area/ sum(clc_change_totals$totArea)*100)

baselineLCdistFig <- clc_baseline_distance %>%
  bind_rows(clc_baseline_distance %>%
              group_by(type, distance) %>%
              summarise(area = sum(area))%>%
              mutate(areaPerc = area/ sum(clc_change_totals$totArea)*100) %>%
              mutate(stratum = 'all land cover')) %>%
  ggplot(aes(x=distance, y = areaPerc, color=type)) +
  geom_point(alpha=0.25) +
  geom_line()+
  scale_x_continuous(breaks=c(2,4,6,8,10), labels = paste('<', c(2,4,6,8,10))) +
  labs(color='Buffer type',
       x = "Buffer distance from water's edge (km)",
       y = "Percentage of total terrestrial area (%)") +
  facet_wrap(~ stratum, scales='free_y', nrow=1,  labeller = label_wrap_gen(width = 24)) +
  scale_color_manual(values = c('#3c0f73',  '#62c8ca','#4582bb'))+
  theme(plot.title = element_text(size=11))
ggsave(out_path("baselineLCdistFig.png"), baselineLCdistFig, width = 35, height=10, units='cm')

#### Figure S1b: share of total net loss by distance band ---------------------
clc_change_totals_net <- clc_change_totals %>%
  filter(str_detect(stratum, 'from|to')) %>%
  mutate(stratum = recode_factor(factor(stratum), "Loss to urban" = "nature -> urban",
                                 "Gain from urban" = "urban -> nature",
                                 "Loss to cropland" = "nature -> cropland",
                                 "Gain from cropland" = "cropland -> nature") ) %>%
  bind_rows(clc_change_country_trans_netChng %>%
              group_by(lc_flow) %>%
              summarise(netChange = sum(netChange)) %>%
              bind_rows(clc_change_country_trans_netChng %>%
                          summarise(netChange = sum(netChange)) %>%
                          mutate(lc_flow = 'total')) %>%
              mutate(stratum = recode_factor(factor(lc_flow),
                                             "cropland" = "(nature -> cropland) - (cropland -> nature)",
                                             "urban" = "(nature -> urban) - (urban -> nature)",
                                             "total" = "(nature -> urban|cropland) - (urban|cropland -> nature)")) %>%
              dplyr::select(stratum, totArea = netChange))

share_change_distance <- clc_change_distance %>%
  filter(str_detect(stratum, '>')) %>%
  filter(type == 'all water') %>%
  group_by(distance, stratum) %>%
  summarise(area = sum(area)/1000000) %>%
  # net loss areas
  bind_rows(clc_netLoss_intensity_distance%>%
              filter(type == 'all water') %>%
              dplyr::select(distance, stratum, area=netLoss)) %>%
  # add net loss areas total
  bind_rows(clc_netLoss_intensity_distance %>%
              filter(type == 'all water')%>%
              group_by(distance) %>%
              summarise(area = sum(netLoss)) %>%
              mutate(stratum = "(nature -> urban|cropland) - (urban|cropland -> nature)") %>%
              dplyr::select(distance, stratum, area)) %>%
  left_join(clc_change_totals_net) %>%
  # make net changes absolute for percentage calculation
  mutate(totArea = abs(totArea)) %>%
  mutate( areaPerc = area/totArea*100)  %>%
  mutate(stratum = factor(stratum, levels = c("urban -> nature",
                                              "cropland -> nature",
                                              "nature -> urban",
                                              "nature -> cropland",
                                              "(nature -> urban) - (urban -> nature)",
                                              "(nature -> cropland) - (cropland -> nature)",
                                              "(nature -> urban|cropland) - (urban|cropland -> nature)")))

shareChangeDistFig <- share_change_distance %>%
  filter(str_detect(stratum, ' - ')) %>%
  ggplot(aes(x=distance, y = areaPerc, color=stratum)) +
  geom_point(alpha=0.25) +
  geom_line()+
  scale_x_continuous(breaks=c(2,4,6,8,10), labels = paste('<', c(2,4,6,8,10))) +
  labs(color='Net loss type',
       x = "Buffer distance from water's edge (km)",
       y = "Percentage of total net loss (%)") +
  theme(plot.title = element_text(size=11),
        legend.position = c(0.6, 0.3),
        legend.background = element_blank())
ggsave(out_path("shareChangeDistFig.png"), shareChangeDistFig, width = 20, height=8, units='cm')
