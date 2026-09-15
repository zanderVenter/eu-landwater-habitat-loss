#### 08 - Figure 6: CLC 6-year epoch trends in net urban/cropland change,
#### per biome
#
# Run from the repo root: Rscript R/08_figure6_epoch_trends.R
# Depends on: R/00_setup.R (sourced below).
# Writes: <OUTPUTS_DIR>/fig6.png
#
# Revision addition (response to Reviewer 2's request for a less discrete
# change signal than a single 2000-2018 window). Reads
# clc_areas_change_epochs_grid_50km.csv (from 05_areas_extract_epochs.js -
# same stratum encoding as the main clc_areas_change_2000_2018_grid_50km.csv,
# landwater*14 + clc_class, just repeated for three 6-year epochs) and joins
# to the same id -> biome lookup Figure 4 uses.
#
# One row per biome (same 6 rows, same order as Figure 4:
# R/05_figure4_biome_panels.R), left column urban-driven net change trend
# across epochs, right column cropland-driven, each row's cropland panel
# paired with a small inset map of that biome (same red-highlight-on-grey
# convention as Figure 4's inset), rather than embedded inside the data
# panel like Figure 4 - a line trend across 3 epochs doesn't reliably leave
# empty plot space the way Figure 4's short bars do, so the map is a third
# grid.arrange column instead of an annotation_custom overlay.
#
# An earlier version of this figure also had a gross (absolute loss/gain
# area) panel; dropped as redundant with the net-change-intensity panel
# below, which is the metric already used everywhere else in this paper,
# and because raw km^2 isn't comparable across differently-sized biomes.
#
# Author: Zander Venter

source("R/00_setup.R")

epoch_levels <- c('2000_2006', '2006_2012', '2012_2018')
epoch_labels <- c('2000-06', '2006-12', '2012-18')

clc_change_epochs <- read_csv(data_path('from_gee/clc_areas_change_epochs_grid_50km.csv')) %>%
  mutate(
    landwater = stratum %/% 14, # see getStratAreas() in 05_areas_extract_epochs.js
    clc_class = (stratum %% 14)
  ) %>%
  filter(clc_class != 0) %>%
  dplyr::select(-'system:index', -'.geo') %>%
  filter(landwater %in% c(1,2,3)) %>%
  mutate(clc_class = factor(clc_class),
         clc_class = recode_factor(clc_class,
                                   "1" = "Urban",
                                   "2" = "Cropland",
                                   "3" = "Grassland",
                                   "4" = "Woodland and forest",
                                   "5" = "Heathland and shrub",
                                   "6" = "Sparsely vegetated",
                                   "7" = "Wetlands",
                                   "8" = "Marine inlets",
                                   "9" = "Rivers and lakes",
                                   "10" = "Loss to urban",
                                   "11" = "Gain from urban",
                                   "12" = "Loss to cropland",
                                   "13" = "Gain from cropland"),
         landwater = recode_factor(factor(landwater),
                                   "1" = "Inland",
                                   "2" = "Coastal",
                                   "3" = "Riparian"),
         epoch = factor(epoch, levels = epoch_levels, labels = epoch_labels)) %>%
  left_join(ecoregion_lookup, by = 'id')

#### Baseline (start-of-epoch) nature area per epoch x landwater x biome -----
# "Nature" here = still-nature at epoch end (stable) + nature lost during
# the epoch (Loss to urban/cropland) - i.e. what was nature at the START of
# that 6-year window, same trick used everywhere else in this pipeline.
get_nature_tot <- function(df) {
  df %>%
    filter(!clc_class %in% c('Marine inlets', 'Rivers and lakes')) %>%
    filter(!clc_class %in% c('Urban', 'Cropland')) %>%
    filter(!str_detect(clc_class, 'Gain')) %>%
    group_by(epoch, landwater, BIOME_NAME) %>%
    summarise(totArea_landwater = sum(area, na.rm = TRUE)/1e6, .groups = 'drop')
}

nature_tot_biome <- get_nature_tot(clc_change_epochs)
nature_tot_all <- get_nature_tot(clc_change_epochs %>% mutate(BIOME_NAME = 'All'))
nature_tot <- bind_rows(nature_tot_biome, nature_tot_all)

#### Gross and net change per epoch x landwater x biome x driver -------------
get_change_summary <- function(df) {
  df %>%
    filter(str_detect(clc_class, 'Loss|Gain')) %>%
    group_by(epoch, landwater, BIOME_NAME, clc_class) %>%
    summarise(change_area = sum(area, na.rm = TRUE)/1e6, .groups = 'drop') %>%
    mutate(clc_base = str_split(clc_class, ' ') %>% map_chr(3),
           clc_lossgain = str_split(clc_class, ' ') %>% map_chr(1))
}

change_summary_biome <- get_change_summary(clc_change_epochs)
change_summary_all <- get_change_summary(clc_change_epochs %>% mutate(BIOME_NAME = 'All'))
change_summary <- bind_rows(change_summary_biome, change_summary_all)

epoch_change <- change_summary %>%
  dplyr::select(epoch, landwater, BIOME_NAME, clc_base, clc_lossgain, change_area) %>%
  pivot_wider(names_from = clc_lossgain, values_from = change_area, values_fill = 0) %>%
  left_join(nature_tot, by = c('epoch', 'landwater', 'BIOME_NAME')) %>%
  mutate(netChange = Gain - Loss,
         change_ratio_netchange = netChange / totArea_landwater * 100)

landwater_pal <- c('Inland' = '#eadc91', 'Riparian' = '#62c8ca', 'Coastal' = '#4582bb')

#### Per-biome panel builder (mirrors R/05_figure4_biome_panels.R) ------------
# Same biome set and row order as Figure 4 - keeps the two figures directly
# comparable and re-uses the same "grey Europe, red highlighted biome"
# inset map convention.
biome_order <- c('All', 'Tundra', 'Boreal Forests/Taiga',
                  'Temperate Broadleaf & Mixed Forests',
                  'Temperate Conifer Forests', 'Mediterranean Woodlands & Scrub')
epoch_change <- epoch_change %>%
  filter(BIOME_NAME %in% biome_order) %>%
  mutate(BIOME_NAME = factor(BIOME_NAME, levels = biome_order))

gridToMap <- grid %>%
  mutate(lon = st_coordinates(st_centroid(geometry))[,1]) %>%
  filter(lon > 2250000)

# Fixed y-range per driver, shared across every biome row, so line
# magnitude is directly comparable between biomes (same logic as Figure
# 4's single shared xlims across all rows) - urban and cropland get
# separate ranges since urban net change is consistently larger in
# magnitude than cropland net change across every biome in this dataset.
ylims_urban <- c(-0.65, 0.05)
ylims_cropland <- c(-0.2, 0.15)

makeEpochPlot <- function(biomeselect, title, legend, xaxis) {

  plot_data <- epoch_change %>% filter(BIOME_NAME == biomeselect)

  pLeft <- plot_data %>%
    filter(clc_base == 'urban') %>%
    ggplot(aes(x = epoch, y = change_ratio_netchange, color = landwater, group = landwater)) +
    geom_hline(yintercept = 0, linetype = 2, color = 'grey60') +
    geom_point(alpha = 0.6) +
    geom_line() +
    scale_color_manual(values = landwater_pal) +
    ylim(ylims_urban) +
    labs(title = paste0(title, ' ', str_wrap(biomeselect, width = 28)),
         x = NULL, y = 'Net change (%)\nurban') +
    theme(
      plot.title = element_text(size = 8),
      plot.margin = margin(t = 5, r = 5, b = 5, l = 8),
      axis.title.y = element_text(size = 8),
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      legend.position = 'none'
    )

  if (legend) {
    pLeft <- pLeft +
      theme(legend.position = c(0.25, 0.2),
            legend.title = element_blank(),
            legend.background = element_blank())
  }

  pRight <- plot_data %>%
    filter(clc_base == 'cropland') %>%
    ggplot(aes(x = epoch, y = change_ratio_netchange, color = landwater, group = landwater)) +
    geom_hline(yintercept = 0, linetype = 2, color = 'grey60') +
    geom_point(alpha = 0.6) +
    geom_line() +
    scale_color_manual(values = landwater_pal) +
    ylim(ylims_cropland) +
    labs(title = '', x = NULL, y = 'Net change (%)\ncropland') +
    theme(
      legend.position = 'none',
      plot.margin = margin(t = 5, r = 5, b = 5, l = 8),
      axis.title.y = element_text(size = 8),
      axis.title.x = element_blank(),
      axis.text.x = element_blank()
    )

  if (xaxis) {
    pLeft <- pLeft + labs(x = 'Epoch') + theme(axis.text.x = element_text())
    pRight <- pRight + labs(x = 'Epoch') + theme(axis.text.x = element_text())
  }

  biomeToMap <- if (biomeselect == 'All') {
    gridToMap
  } else {
    gridToMap %>%
      left_join(ecoregion_lookup) %>%
      filter(BIOME_NAME == biomeselect)
  }

  pMap <- biomeToMap %>%
    ggplot() +
    # Grey Europe backdrop (grid cells outside the focal biome) + the
    # highlighted biome on top - same convention as Figure 4. The grey here
    # is the map CONTENT (context), not a ggplot panel/plot background - the
    # latter is set to NA below so no grey rectangle appears around the map.
    geom_sf(data = gridToMap %>% st_union(), fill = '#7f7f7f', color = NA) +
    geom_sf(fill = '#f6978f', color = NA) +
    theme_void() +
    theme(panel.background = element_rect(fill = NA, color = NA),
          plot.background = element_rect(fill = NA, color = NA))

  p <- grid.arrange(pLeft, pRight, pMap, ncol = 3, widths = c(1, 1, 0.6),
                     padding = unit(0, "line"), newpage = F)

  return(p)
}

#### Assemble and save -------------------------------------------------------
ep1 <- makeEpochPlot('All', 'a)', FALSE, FALSE)
ep2 <- makeEpochPlot('Tundra', 'b)', TRUE, FALSE)
ep3 <- makeEpochPlot('Boreal Forests/Taiga', 'c)', FALSE, FALSE)
ep4 <- makeEpochPlot('Temperate Broadleaf & Mixed Forests', 'd)', FALSE, FALSE)
ep5 <- makeEpochPlot('Temperate Conifer Forests', 'e)', FALSE, FALSE)
ep6 <- makeEpochPlot('Mediterranean Woodlands & Scrub', 'f)', FALSE, TRUE)
fig6 <- grid.arrange(ep1, ep2, ep3, ep4, ep5, ep6, ncol = 1,
                      heights = c(1,1,1,1,1,1.2), padding = unit(0, "line"), newpage = F)

ggsave(out_path("fig6.png"), fig6, width = 20, height = 24, units = 'cm', bg = 'white')
