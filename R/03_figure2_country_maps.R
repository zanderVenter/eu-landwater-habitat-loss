#### 03 - Figure 2: net change maps (riparian/coastal) + country ratio panels
#
# Run from the repo root: Rscript R/03_figure2_country_maps.R
# Depends on: R/02_table1_accounting.R (sourced below, which itself sources
# R/00_setup.R) for change_account_country_to_export.
# Writes: <OUTPUTS_DIR>/fig2.png
#
# Panels: (a) riparian zone net change map, (b) coastal zone net change map,
# (c) country-level net change (riparian & coastal vs. their components),
# (d) ratio of land-water vs. inland net loss intensity per country.
#
# Author: Zander Venter

source("R/02_table1_accounting.R")

#### Grid-level net change ratio (for panels a, b maps) ----------------------
clc_nature_grid_tot <- clc_change %>%
  # Terrestrial surfaces
  filter(!clc_class %in% c('Marine inlets', 'Rivers and lakes') ) %>%
  # Nature land cover
  filter(!clc_class %in% c('Urban', 'Cropland')) %>%
  # Excluding habitats recovered since 2000 (i.e. they were urban or cropland in 2000)
  filter(!str_detect(clc_class, 'Gain')) %>%
  group_by(landwater, id) %>%
  summarise(totArea_landwater = sum(area, na.rm=T)/1000000) %>%
  ungroup() %>%
  mutate(percArea_landwater = totArea_landwater/sum(totArea_landwater)*100)

# Ratio net loss with urban and cropland combined
clc_change_comb_grid_ratio <- clc_change %>%
  filter(str_detect(clc_class, 'Loss|Gain')) %>%
  mutate(clc_lossgain = str_split(clc_class, ' ') %>% map_chr(1)) %>%
  group_by(clc_lossgain, landwater, id)%>%
  summarise(change_area = sum(area, na.rm=T)/1000000) %>%
  left_join(clc_nature_grid_tot, by = c('id', 'landwater')) %>%
  mutate(change_ratio = change_area / totArea_landwater * 100)%>%
  ungroup() %>%
  dplyr::select(id, landwater, clc_lossgain, change_area) %>%
  pivot_wider(names_from=clc_lossgain, values_from=change_area, values_fill = 0) %>%
  mutate(netChange = Gain - Loss,
         recoverPerc = Gain / Loss * 100) %>%
  left_join(clc_nature_grid_tot, by = c('id', 'landwater')) %>%
  mutate(change_ratio_netchange = netChange / totArea_landwater * 100,
         change_ratio_loss = Loss / totArea_landwater * 100,
         change_ratio_gain = Gain / totArea_landwater * 100)

#### Panel a: riparian zone net change map ------------------------------------
gm1 <- grid %>%
  left_join(clc_change_comb_grid_ratio %>%
              filter(landwater == 'Riparian')) %>%
  filter(!is.na(netChange)) %>%
  ggplot() +
  geom_sf(data = countries, fill = "grey", alpha= 0.2,color = '#172b24',size = 0.1)+
  geom_sf( aes(fill=netChange),  color=NA, alpha=0.7) +
  scale_fill_gradientn(colours = bam(10),
                       limits=c(-5,5),
                       oob = scales::squish) +
  theme_void() +
  labs(title = 'a) Riparian zone',
       fill = 'Net change\n(% of baseline)') +
  xlim(2590000,7370000) +
  ylim(1420000,5457000) +
  theme(legend.position= c(0.8, 0.6))+
  theme(
    plot.title = element_text(size = 10),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 6)
  )

#### Panel b: coastal zone net change map --------------------------------------
gm2 <- grid %>%
  left_join(clc_change_comb_grid_ratio %>%
              filter(landwater == 'Coastal')) %>%
  filter(!is.na(netChange)) %>%
  ggplot() +
  geom_sf(data = countries, fill = "grey", alpha= 0.2,color = '#172b24',size = 0.1)+
  geom_sf( aes(fill=netChange),  color=NA, alpha=0.7) +
  scale_fill_gradientn(colours = bam(10),
                       limits=c(-5,5),
                       oob = scales::squish) +
  theme_void() +
  labs(title = 'b) Coastal zone',
       fill = 'Net change\n(% of baseline)') +
  xlim(2590000,7370000) +
  ylim(1420000,5457000) +
  theme(legend.position='none')+
  theme(
    plot.title = element_text(size = 10),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 6)
  )

#### Panels c, d: country-level net change and land-water:inland ratio ---------
change_acount_country_ratio <- change_account_country_to_export %>%
  filter(metric == 'area_perc') %>%
  # 1. Replace 0s with NA in the specified columns - countries without coastline
  mutate(
    urban_Coastal = na_if(urban_Coastal, 0),
    cropland_Coastal = na_if(cropland_Coastal, 0)
  ) %>%
  # 2. Use rowMeans with na.rm = TRUE to calculate averages correctly
  mutate(
    urban_change_lw = rowMeans(across(c(urban_Riparian, urban_Coastal)), na.rm = TRUE),
    cropland_change_lw = rowMeans(across(c(cropland_Riparian, cropland_Coastal)), na.rm = TRUE),
    riparian_change = rowMeans(across(c(urban_Riparian, cropland_Riparian)), na.rm = TRUE),
    coastal_change = rowMeans(across(c(urban_Coastal, cropland_Coastal)), na.rm = TRUE),
    landwater_change = rowMeans(across(c(urban_Riparian, cropland_Riparian,urban_Coastal, cropland_Coastal)), na.rm = TRUE)
  ) %>%
  # 3. Calculate Ratios
  mutate(
    inland_avg = rowMeans(across(c(urban_Inland, cropland_Inland)), na.rm = TRUE),
    landwater_avg = rowMeans(across(c(urban_Riparian, cropland_Riparian, urban_Coastal, cropland_Coastal)), na.rm = TRUE) ,
    # Overall ratio (Interface Mean / Inland Mean)
    ratio = landwater_avg %>% abs() /
      inland_avg %>% abs(),

    ratio_urb      = abs(urban_change_lw) / abs(urban_Inland),
    ratio_cropland = abs(cropland_change_lw) / abs(cropland_Inland),
    ratio_riparian = abs(riparian_change) / abs(inland_avg),
    ratio_coastal  = abs(coastal_change) / abs(inland_avg)
  ) %>%
  # 4. Clean up results
  filter(!is.na(ratio) & ratio != Inf & ratio != 0) %>%
  # Adjusted the final ifelse to check for NA instead of 0
  mutate(ratio_riparian = ifelse(is.na(ratio_coastal), ratio, ratio_riparian))

cp1 <- change_acount_country_ratio %>%
  mutate(orderVar = landwater_change) %>%
  mutate(label = ifelse(!is.na(coastal_change), paste0(round(landwater_change,1), ' (C: ', round(coastal_change,1), '; R: ', round(riparian_change,1), ')'),
                        paste0(round(landwater_change,1), ' (R: ', round(riparian_change,1), ')')))%>%
  gather(key, val, landwater_change, coastal_change, riparian_change)%>%
  mutate(key = recode(key,
                      "landwater_change" = "Riparian & Coastal",
                      "coastal_change" = "Coastal",
                      "riparian_change" = "Riparian"))  %>%
  filter(val != 0) %>%
  ggplot(aes(y=reorder(country, orderVar), x=val, color=key)) +
  geom_point() +
  geom_text(aes(label = label, x=1.5, y = reorder(country, orderVar)), hjust=-0.1, size=2.5, inherit.aes=F) +
  geom_vline(xintercept = 0, linetype = 2)  +
  xlim(-4.5,4.5)+
  labs(x = 'Net change percentage of baseline nature area (%)',
       title = 'c') +
  theme(axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.position = c(0.2,0.5))

cp2 <- change_acount_country_ratio %>%
  mutate(orderVar = ratio) %>%
  mutate(label = ifelse(!is.na(coastal_change), paste0(round(ratio,1), ' (C: ', round(ratio_coastal,1), '; R: ', round(ratio_riparian,1), ')'),
                        paste0(round(ratio,1), ' (R: ', round(ratio_riparian,1), ')'))) %>%
  gather(key, val, ratio, ratio_riparian, ratio_coastal) %>%
  mutate(key = recode(key,
                             "ratio" = "Riparian & Coastal",
                             "ratio_coastal" = "Coastal",
                             "ratio_riparian" = "Riparian")) %>%
  filter(val != 0) %>%
  ggplot(aes(y=reorder(country, orderVar), x=val, color=key)) +
  geom_point() +
  geom_text(aes(label = label, x=200, y = reorder(country, orderVar)), hjust=-0.1, size=2.5, inherit.aes=F) +
  geom_vline(xintercept = 1, linetype = 2) +
  scale_x_log10(limits = c(0.03,15000)) +
  scale_color_manual(values = c( '#62c8ca','#4582bb', '#000000')) +
  labs(x = 'Ratio net loss land-water : net loss inland',
       title = 'd') +
  theme(axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.position = 'none')

#### Assemble and save -------------------------------------------------------
netChangeRatioFig_top <- grid.arrange(gm1, gm2,   nrow=2, heights=c(1,1), padding = unit(0, "line"), newpage = F)
netChangeRatioFig <- grid.arrange(netChangeRatioFig_top, cp1, cp2,  ncol=3, widths=c(1.2,1,1), padding = unit(0, "line"), newpage = F)

ggsave(out_path("fig2.png"), netChangeRatioFig, width = 32, height=18, units='cm')
