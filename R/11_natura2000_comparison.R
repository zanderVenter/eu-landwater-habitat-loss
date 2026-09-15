#### 11 - Figure S: habitat change intensity inside vs. outside Natura2000
#### sites, by landwater zone
#
# Run from the repo root: Rscript R/11_natura2000_comparison.R
# Depends on: R/00_setup.R (sourced below).
# Writes: <OUTPUTS_DIR>/figS_natura2000.png
#
# Revision addition (response to Reviewer 2's request for a stronger link
# to EU conservation policy - see the revision plan for why Natura2000 got
# a real quantitative comparison while WFD/MSFD are discussion-only: unlike
# those, Natura2000 has a clean inside/outside split). Reads
# clc_areas_change_2000_2018_natura2000.csv (from
# 08_areas_extract_natura2000.js). stratum encodes landwaterProtected*14 +
# clc_class, where landwaterProtected is 1-6 (see that script's header):
# 1 inland-unprotected, 2 inland-protected, 3 coastal-unprotected,
# 4 coastal-protected, 5 riparian-unprotected, 6 riparian-protected.
#
# Author: Zander Venter

source("R/00_setup.R")

# Decode landwaterProtected (1-6) into landwater x protected
landwater_protected_lookup <- tribble(
  ~landwaterProtected, ~landwater,  ~protected,
  1,                    "Inland",    "Unprotected",
  2,                    "Inland",    "Protected",
  3,                    "Coastal",   "Unprotected",
  4,                    "Coastal",   "Protected",
  5,                    "Riparian",  "Unprotected",
  6,                    "Riparian",  "Protected"
)

clc_change_natura2000 <- read_csv(data_path('from_gee/clc_areas_change_2000_2018_natura2000.csv')) %>%
  mutate(
    landwaterProtected = stratum %/% 14, # see getStratAreas() in 08_areas_extract_natura2000.js
    clc_class = (stratum %% 14)
  ) %>%
  filter(clc_class != 0, landwaterProtected %in% 1:6) %>%
  dplyr::select(-'system:index', -'.geo') %>%
  left_join(landwater_protected_lookup, by = 'landwaterProtected') %>%
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
         landwater = factor(landwater, levels = c('Inland', 'Coastal', 'Riparian')),
         protected = factor(protected, levels = c('Unprotected', 'Protected')))

#### Baseline nature area per landwater x protected (same trick as elsewhere) --
nature_tot <- clc_change_natura2000 %>%
  filter(!clc_class %in% c('Marine inlets', 'Rivers and lakes')) %>%
  filter(!clc_class %in% c('Urban', 'Cropland')) %>%
  filter(!str_detect(clc_class, 'Gain')) %>%
  group_by(landwater, protected) %>%
  summarise(totArea = sum(area, na.rm = TRUE)/1e6, .groups = 'drop')

#### Gross and net change per landwater x protected x driver -------------------
natura2000_change_ratio <- clc_change_natura2000 %>%
  filter(str_detect(clc_class, 'Loss|Gain')) %>%
  group_by(landwater, protected, clc_class) %>%
  summarise(change_area = sum(area, na.rm = TRUE)/1e6, .groups = 'drop') %>%
  mutate(clc_base = str_split(clc_class, ' ') %>% map_chr(3),
         clc_lossgain = str_split(clc_class, ' ') %>% map_chr(1)) %>%
  dplyr::select(landwater, protected, clc_base, clc_lossgain, change_area) %>%
  pivot_wider(names_from = clc_lossgain, values_from = change_area, values_fill = 0) %>%
  left_join(nature_tot, by = c('landwater', 'protected')) %>%
  mutate(netChange = Gain - Loss,
         change_ratio_netchange = netChange / totArea * 100)

protected_pal <- c('Unprotected' = '#888d80', 'Protected' = '#556B2F')

# Panels a, b: urban-driven and cropland-driven net change. Bare letter
# labels only - the explanatory caption text lives externally in the
# manuscript.
make_natura_panel <- function(driver, panel_label) {
  natura2000_change_ratio %>%
    filter(clc_base == driver) %>%
    ggplot(aes(x = landwater, y = change_ratio_netchange, fill = protected)) +
    geom_hline(yintercept = 0, linetype = 2, color = 'grey60') +
    geom_col(position = position_dodge(width = 0.7), width = 0.65) +
    scale_fill_manual(values = protected_pal) +
    labs(x = NULL,
         y = "Net change (% of baseline nature area)",
         fill = 'Natura2000',
         title = panel_label) +
    theme(legend.position = 'bottom')
}

panel_a <- make_natura_panel('urban', 'a') + theme(legend.position = 'none')
panel_b <- make_natura_panel('cropland', 'b') + theme(axis.title.y = element_blank())

#### Panels c, d: protected vs. unprotected net loss intensity ratio ----------
# Companion to a/b - abs(Protected) / abs(Unprotected) per landwater zone,
# points rather than bars since this is a ratio (same convention as panel c
# in R/10_figureS_multidataset.R). A ratio below 1 means protected sites
# lost less intensely than unprotected land in the same zone.
protected_ratio <- natura2000_change_ratio %>%
  dplyr::select(landwater, protected, clc_base, change_ratio_netchange) %>%
  pivot_wider(names_from = protected, values_from = change_ratio_netchange) %>%
  mutate(ratio = abs(Protected) / abs(Unprotected)) %>%
  # Drop ratios with a near-zero (noise-level) denominator: Coastal/cropland
  # unprotected net change is 0.000119% (essentially no signal either way -
  # see the console summary above), so its ratio (~650) is a division-by-
  # noise artifact, not a real effect, and would otherwise dominate the log
  # axis and squash every other point. 0.005% is a generous floor - every
  # other |Unprotected| value in this dataset is at least 0.03%.
  filter(abs(Unprotected) > 0.005)

landwater_pal <- c('Inland' = '#eadc91', 'Riparian' = '#62c8ca', 'Coastal' = '#4582bb')

make_natura_ratio_panel <- function(driver, panel_label) {
  # Log scale: a near-zero unprotected-cropland denominator in one zone
  # (Coastal, cropland - see the console summary above, ~0.0001) sends that
  # single ratio into the hundreds, which would otherwise swamp the other
  # five points on a linear axis. Same fix as the country-level ratio panel
  # in Figure 2 (R/03_figure2_country_maps.R), which hits the same issue.
  protected_ratio %>%
    filter(clc_base == driver) %>%
    ggplot(aes(x = landwater, y = ratio, color = landwater)) +
    geom_hline(yintercept = 1, linetype = 2, color = 'grey60') +
    geom_point(size = 3) +
    scale_color_manual(values = landwater_pal) +
    scale_y_log10() +
    labs(x = NULL,
         y = "Net loss intensity ratio\n(protected : unprotected)",
         title = panel_label) +
    theme(legend.position = 'none')
}

panel_c <- make_natura_ratio_panel('urban', 'c')
panel_d <- make_natura_ratio_panel('cropland', 'd') + theme(axis.title.y = element_blank())

figS_natura2000 <- grid.arrange(panel_a, panel_b, panel_c, panel_d, ncol = 2, padding = unit(0, "line"), newpage = F)

ggsave(out_path("figS_natura2000.png"), figS_natura2000, width = 22, height = 24, units = 'cm')

# Quick console summary for the results-text draft
natura2000_change_ratio %>%
  dplyr::select(landwater, protected, clc_base, change_ratio_netchange) %>%
  arrange(clc_base, landwater, protected) %>%
  print(n = Inf)
