#### 09 - Figure S: habitat change intensity by major European catchment
#
# Run from the repo root: Rscript R/09_figureS_basins.R
# Depends on: R/00_setup.R (sourced below).
# Writes: <OUTPUTS_DIR>/figS_basins.png
#
# Revision addition (response to Reviewer 2's request for a major-watershed
# breakdown). Reads clc_areas_change_2000_2018_basins.csv (from
# 06_areas_extract_basins.js - same stratum encoding and metric as Figure
# 4's biome breakdown: net/gross urban and cropland change as a % of
# baseline nature area, by landwater zone).
#
# Panels: (a) basin-level net change (% of baseline nature area), combining
# urban + cropland, shown for Coastal, Riparian, and their combined
# "Riparian & Coastal" average; (b) ratio of net loss intensity, land-water
# zones vs. inland, same combination; (c) basin map of the same "Riparian &
# Coastal" net change value as panel a; (d) basin map of the same ratio as
# panel b. This directly mirrors R/03_figure2_country_maps.R's panels c/d
# (country-level version of the same statistic) - see that script for the
# combining logic (rowMeans of urban/cropland x Riparian/Coastal). Basin
# names are labelled on both maps, so the reader can look up the spatial
# location of any basin named in panels a/b directly from whichever map
# they're looking at.
#
# Author: Zander Venter

source("R/00_setup.R")
library(ggrepel)

clc_change_basins <- read_csv(data_path('from_gee/clc_areas_change_2000_2018_basins.csv')) %>%
  mutate(
    landwater = stratum %/% 14, # see getStratAreas() in 06_areas_extract_basins.js
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
                                   "3" = "Riparian"))

#### Baseline nature area per basin x landwater (same trick as Figure 4) ------
basin_nature_tot <- clc_change_basins %>%
  filter(!clc_class %in% c('Marine inlets', 'Rivers and lakes')) %>%
  filter(!clc_class %in% c('Urban', 'Cropland')) %>%
  filter(!str_detect(clc_class, 'Gain')) %>%
  group_by(basin_name, landwater) %>%
  summarise(totArea_landwater = sum(area, na.rm = TRUE)/1e6, .groups = 'drop')

#### Net change per basin x landwater x driver (% of baseline, signed) --------
# Signed (not flipped to a "loss-positive" convention) to match Figure 2's
# panels c/d, which this figure's panels a-d are built to mirror.
basin_change_ratio <- clc_change_basins %>%
  filter(str_detect(clc_class, 'Loss|Gain')) %>%
  group_by(basin_name, landwater, clc_class) %>%
  summarise(change_area = sum(area, na.rm = TRUE)/1e6, .groups = 'drop') %>%
  mutate(clc_base = str_split(clc_class, ' ') %>% map_chr(3),
         clc_lossgain = str_split(clc_class, ' ') %>% map_chr(1)) %>%
  dplyr::select(basin_name, landwater, clc_base, clc_lossgain, change_area) %>%
  pivot_wider(names_from = clc_lossgain, values_from = change_area, values_fill = 0) %>%
  left_join(basin_nature_tot, by = c('basin_name', 'landwater')) %>%
  mutate(netChange = Gain - Loss,
         change_ratio_netchange = netChange / totArea_landwater * 100)

#### Combine urban + cropland, Riparian + Coastal, mirroring Figure 2 ---------
# change_account_country_ratio's construction (R/03_figure2_country_maps.R):
# "Riparian & Coastal" = rowMeans of urban/cropland x Riparian/Coastal;
# ratio = abs(that) / abs(inland average).
basin_wide <- basin_change_ratio %>%
  dplyr::select(basin_name, landwater, clc_base, change_ratio_netchange) %>%
  pivot_wider(names_from = c(clc_base, landwater), values_from = change_ratio_netchange)

basin_combined <- basin_wide %>%
  mutate(
    urban_Coastal = na_if(urban_Coastal, 0),
    cropland_Coastal = na_if(cropland_Coastal, 0)
  ) %>%
  mutate(
    riparian_change = rowMeans(across(c(urban_Riparian, cropland_Riparian)), na.rm = TRUE),
    coastal_change = rowMeans(across(c(urban_Coastal, cropland_Coastal)), na.rm = TRUE),
    landwater_change = rowMeans(across(c(urban_Riparian, cropland_Riparian, urban_Coastal, cropland_Coastal)), na.rm = TRUE),
    inland_avg = rowMeans(across(c(urban_Inland, cropland_Inland)), na.rm = TRUE)
  ) %>%
  mutate(
    ratio = abs(landwater_change) / abs(inland_avg),
    ratio_riparian = abs(riparian_change) / abs(inland_avg),
    ratio_coastal = abs(coastal_change) / abs(inland_avg)
  ) %>%
  filter(!is.na(ratio) & is.finite(ratio) & ratio != 0) %>%
  mutate(ratio_riparian = ifelse(is.na(ratio_coastal), ratio, ratio_riparian))

combined_pal <- c('Riparian & Coastal' = '#000000', 'Coastal' = '#4582bb', 'Riparian' = '#62c8ca')

#### Panel a: net change (% of baseline nature area) --------------------------
# Basin axis ordered largest to smallest "Riparian & Coastal" value, own
# order per panel (not shared with panel b) - same convention as Figure 2's
# cp1/cp2. Legend placed inside the plot area (not below it) so panels a
# and b end up the same height.
panel_a <- basin_combined %>%
  mutate(basin_name = fct_reorder(basin_name, landwater_change)) %>%
  gather(key, val, landwater_change, coastal_change, riparian_change) %>%
  mutate(key = recode(key,
                      "landwater_change" = "Riparian & Coastal",
                      "coastal_change" = "Coastal",
                      "riparian_change" = "Riparian")) %>%
  filter(val != 0) %>%
  ggplot(aes(y = basin_name, x = val, color = key)) +
  geom_vline(xintercept = 0, linetype = 2, color = 'grey60') +
  geom_point(size = 2, alpha = 0.85) +
  scale_color_manual(values = combined_pal) +
  labs(x = 'Net change (% of baseline nature area)',
       y = NULL,
       title = 'a') +
  theme(axis.text.y = element_text(size = 7),
        legend.title = element_blank(),
        legend.background = element_blank(),
        # Top-left corner: basins with the largest (least-negative)
        # combined value are plotted at the top, clustered near x = 0, so
        # the far-negative-x region up there is empty plot space.
        legend.position = c(0.2, 0.94))

#### Panel b: ratio of net loss intensity, land-water zone : inland -----------
panel_b <- basin_combined %>%
  mutate(basin_name = fct_reorder(basin_name, ratio)) %>%
  gather(key, val, ratio, ratio_riparian, ratio_coastal) %>%
  mutate(key = recode(key,
                      "ratio" = "Riparian & Coastal",
                      "ratio_coastal" = "Coastal",
                      "ratio_riparian" = "Riparian")) %>%
  filter(val != 0) %>%
  ggplot(aes(y = basin_name, x = val, color = key)) +
  geom_vline(xintercept = 1, linetype = 2, color = 'grey60') +
  geom_point(size = 2, alpha = 0.85) +
  scale_x_log10() +
  scale_color_manual(values = combined_pal) +
  labs(x = 'Ratio net loss intensity\n(land-water zone : inland)',
       y = NULL,
       title = 'b') +
  theme(axis.text.y = element_text(size = 7), legend.position = 'none')

#### Panels c, d: basin maps of the same two "Riparian & Coastal" values ------
sf_use_s2(FALSE) # avoids a spurious OGR/s2 error on this basins layer's centroids
basins <- st_read(data_path('basins_eu_hydro_v013.shp'), quiet = TRUE) %>%
  filter(!is.na(basin_name)) %>%
  # Some of the 33 basin polygons carry a Z dimension (leftover from the
  # source EU-Hydro FGDBs) and some don't - sf's geom_sf() rendering breaks
  # when a single layer mixes XY and XYZ geometries ("number of columns of
  # matrices must match"). Drop Z from all of them so every geometry is
  # plain XY.
  st_zm(drop = TRUE)
# Already the same CRS as grid (ETRS89-LAEA / EPSG:3035) - st_crs<- assigns
# it directly rather than st_transform()-ing, which (even as a no-op
# reprojection to an already-identical CRS) corrupts this particular
# layer's geometries and breaks st_centroid() downstream.
st_crs(basins) <- st_crs(grid)
basins <- basins %>% left_join(basin_combined, by = 'basin_name')

basin_labels <- basins %>%
  st_centroid() %>%
  mutate(lon = st_coordinates(geometry)[,1], lat = st_coordinates(geometry)[,2]) %>%
  st_drop_geometry()

map_xlim <- c(2590000, 7370000)
map_ylim <- c(1420000, 5457000)

# Panel c: net change map - diverging around 0, same bam() palette Figure 2
# uses, limits set to this dataset's actual range (all-negative here, but
# left diverging/symmetric around 0 rather than sequential, since a basin
# with net gain is possible in principle even though none occur here).
panel_c <- basins %>%
  ggplot() +
  geom_sf(data = countries, fill = "grey", alpha = 0.2, color = '#172b24', linewidth = 0.1) +
  geom_sf(aes(fill = landwater_change), color = 'white', linewidth = 0.15) +
  geom_text_repel(data = basin_labels, aes(x = lon, y = lat, label = basin_name),
                   size = 2, color = 'grey10', segment.size = 0.2, segment.color = 'grey40',
                   max.overlaps = 20, bg.color = 'white', bg.r = 0.1) +
  scale_fill_gradientn(colours = bam(10), limits = c(-2.5, 2.5), oob = scales::squish) +
  theme_void() +
  labs(title = 'c) Riparian & Coastal - net change',
       fill = 'Net change\n(% of baseline)') +
  xlim(map_xlim) + ylim(map_ylim) +
  theme(legend.position = c(0.85, 0.6),
        plot.title = element_text(size = 10),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 6),
        panel.background = element_rect(fill = 'white', color = NA),
        plot.background = element_rect(fill = 'white', color = NA))

# Panel d: ratio map - log10(ratio), diverging around ratio = 1 (log10 = 0),
# legend breaks back-transformed to the original ratio scale.
ratio_breaks <- c(0.1, 0.5, 1, 5, 20, 40)
panel_d <- basins %>%
  mutate(log_ratio = log10(ratio)) %>%
  ggplot() +
  geom_sf(data = countries, fill = "grey", alpha = 0.2, color = '#172b24', linewidth = 0.1) +
  geom_sf(aes(fill = log_ratio), color = 'white', linewidth = 0.15) +
  geom_text_repel(data = basin_labels, aes(x = lon, y = lat, label = basin_name),
                   size = 2, color = 'grey10', segment.size = 0.2, segment.color = 'grey40',
                   max.overlaps = 20, bg.color = 'white', bg.r = 0.1) +
  scale_fill_gradientn(colours = bam(10), limits = c(-1.7, 1.7), oob = scales::squish,
                       breaks = log10(ratio_breaks), labels = ratio_breaks) +
  theme_void() +
  labs(title = 'd) Riparian & Coastal - loss ratio',
       fill = 'Ratio net loss\n(zone : inland)') +
  xlim(map_xlim) + ylim(map_ylim) +
  theme(legend.position = c(0.85, 0.6),
        plot.title = element_text(size = 10),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 6),
        panel.background = element_rect(fill = 'white', color = NA),
        plot.background = element_rect(fill = 'white', color = NA))

#### Assemble and save -------------------------------------------------------
dotplots <- grid.arrange(panel_a, panel_b, ncol = 2, padding = unit(0, "line"), newpage = F)
maps <- grid.arrange(panel_c, panel_d, nrow = 2, padding = unit(0, "line"), newpage = F)
figS_basins <- grid.arrange(dotplots, maps, ncol = 2, widths = c(2, 1), padding = unit(0, "line"), newpage = F)

ggsave(out_path("figS_basins.png"), figS_basins, width = 40, height = 24, units = 'cm', bg = 'white')
