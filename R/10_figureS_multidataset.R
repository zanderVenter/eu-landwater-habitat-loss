#### 10 - Figure S: per-year net change by landwater zone, CLC vs. Potapov
#### vs. GLC-FCS30D, plus the class-definition crosswalk table
#
# Run from the repo root: Rscript R/10_figureS_multidataset.R
# Depends on: R/00_setup.R (sourced below).
# Writes: <OUTPUTS_DIR>/figS_multidataset.png,
#         <OUTPUTS_DIR>/table_S3_class_crosswalk.csv
#
# Revision addition (response to Reviewer 2's concern that CLC's 100m MMU
# may miss changes in narrow riparian corridors). Reads the per-country
# CSVs from 07_areas_extract_multidata.js (folder
# data/from_gee/multidata_countries/), sums to continental totals, and
# compares net change per year across the three datasets. See that
# script's header for the full list of comparability caveats (each dataset
# uses its own baseline and endpoint classification - Potapov's baseline
# comes from a separate GLAD product, GLCLU2020 v2 LCLUC_2000, since its
# built-up/cropland products don't carry one of their own; Potapov built-up
# has no abandonment signal; Potapov cropland's endpoint year is 2019, not
# 2018, etc.) - those caveats are why the per-year normalisation below uses
# a different observation-window length per dataset x driver combination
# rather than a single /18 for everything.
#
# Author: Zander Venter

source("R/00_setup.R")

# Observation window length (years) actually covered by each dataset's
# change signal - see 07_areas_extract_multidata.js's header for why these
# differ. Getting this right matters: dividing by the wrong number of years
# would make one dataset's rate look artificially higher/lower than the
# others just from a mismatched window, not a real difference in the
# underlying change.
dataset_years <- tribble(
  ~dataset,     ~clc_base,  ~years,
  "clc",        "urban",    18, # 2000-2018
  "clc",        "cropland", 18,
  "potapov",    "urban",    20, # 2000 (GLCLU2020 v2 LCLUC_2000) - 2020 (Builtup_type expansion)
  "potapov",    "cropland", 19, # 2000 (GLCLU2020 v2 LCLUC_2000) - 2019 (Global_cropland_2019)
  "glc_fcs30d", "urban",    18, # 2000-2018 (b1 - b19)
  "glc_fcs30d", "cropland", 18
)

multidata_files <- list.files(data_path('from_gee/multidata_countries'), full.names = TRUE)
clc_change_multidata <- read_csv(multidata_files) %>%
  mutate(
    landwater = stratum %/% 2, # see getStratAreas() in 07_areas_extract_multidata.js
    flag = stratum %% 2
  ) %>%
  filter(flag == 1) %>% # flag==0 is the unchanged complement, not needed here
  dplyr::select(-'system:index', -'.geo', -stratum, -flag) %>%
  filter(landwater %in% c(1,2,3)) %>%
  mutate(landwater = recode_factor(factor(landwater),
                                   "1" = "Inland",
                                   "2" = "Coastal",
                                   "3" = "Riparian"),
         clc_base = ifelse(str_detect(change_type, 'urban'), 'urban', 'cropland'),
         clc_lossgain = ifelse(str_detect(change_type, '^loss'), 'Loss', 'Gain'))

#### Continent-wide net change per year, by dataset x driver x landwater -----
multidata_netchange <- clc_change_multidata %>%
  group_by(dataset, clc_base, clc_lossgain, landwater) %>%
  summarise(area = sum(area, na.rm = TRUE)/1e6, .groups = 'drop') %>%
  pivot_wider(names_from = clc_lossgain, values_from = area, values_fill = 0) %>%
  # Potapov built-up has no Gain column at all (no abandonment signal - see
  # 07_areas_extract_multidata.js header) - values_fill above already
  # covers this, Gain is simply 0 for potapov x urban.
  mutate(netChange = Gain - Loss) %>%
  left_join(dataset_years, by = c('dataset', 'clc_base')) %>%
  mutate(netChange_per_year = netChange / years,
         dataset = recode_factor(factor(dataset),
                                 "clc" = "CLC",
                                 "potapov" = "Potapov/GLAD",
                                 "glc_fcs30d" = "GLC-FCS30D"),
         clc_base = recode_factor(factor(clc_base), "urban" = "Urban-driven", "cropland" = "Cropland-driven"))

dataset_pal <- c('CLC' = '#172b24', 'Potapov/GLAD' = '#4582bb', 'GLC-FCS30D' = '#e6f602')

# Potapov/GLAD's urban layer is excluded throughout (panels a and c): GLAD's
# own documentation defines built-up as "pixels that include man-made
# surfaces, even if such surfaces do not dominate within the pixel" (no
# dominance threshold, unlike CLC/GLC-FCS30D's conventional classification)
# and explicitly does not map built-up loss at all. That's a real,
# documented definitional mismatch, not a data error - checked against the
# raw continent totals (nature -> urban loss, now that Potapov's own
# GLCLU2020 v2 baseline correctly restricts this to nature-only pixels -
# see 07_areas_extract_multidata.js): CLC and GLC-FCS30D still agree
# closely (6,871 vs. 9,698 km^2), Potapov claims 51,871 km^2 (5-7x higher -
# down from 111,928 km^2 before the baseline fix, since that also fixed a
# separate bug where cropland -> urban was being miscounted as nature ->
# urban; the remaining excess reflects GLAD's inclusive definition itself).
# Left in for cropland (panel b), where Potapov and GLC-FCS30D closely
# agree with each other (130,960 vs. 143,980 km^2) and both diverge from
# CLC by a similar factor - two independent products agreeing is itself
# worth reporting, unlike the urban case.
plot_data <- multidata_netchange %>%
  filter(!(dataset == 'Potapov/GLAD' & clc_base == 'Urban-driven'))

# Panel a: urban-driven, panel b: cropland-driven. Bare letter labels only -
# the explanatory caption text lives externally in the manuscript.
make_multidata_panel <- function(driver, panel_label) {
  plot_data %>%
    filter(clc_base == driver) %>%
    ggplot(aes(x = landwater, y = netChange_per_year, fill = dataset)) +
    geom_hline(yintercept = 0, linetype = 2, color = 'grey60') +
    geom_col(position = position_dodge(width = 0.7), width = 0.65) +
    scale_fill_manual(values = dataset_pal, drop = FALSE) +
    labs(x = NULL,
         y = expression("Net change per year ("~km^2~yr^-1~")"),
         fill = 'Dataset',
         title = panel_label) +
    theme(legend.position = 'bottom')
}

panel_a <- make_multidata_panel('Urban-driven', 'a') + theme(legend.position = 'none')
panel_b <- make_multidata_panel('Cropland-driven', 'b')

#### Panel c: land-water zone vs. inland net loss ratio, by dataset -----------
# Mirrors the abstract's headline CLC-based statistic ("net habitat loss
# intensities 2.6-fold and 1.4-fold greater [in coastal and riparian zones,
# respectively] than inland areas") - here computed per dataset, to see
# whether Potapov/GLAD and GLC-FCS30D show the same disproportionate
# coastal/riparian pattern CLC does. "Combined" (urban+cropland together)
# net change per zone is normalised by CLC's own baseline nature area per
# zone (the only baseline available across all three datasets, since
# Potapov/GLC-FCS30D area extraction here targeted change layers only, not
# a baseline-nature stratum of their own) - so this holds the denominator
# fixed and lets the numerator (net change) vary by dataset, comparable in
# spirit to, though not identical in construction to, the abstract's
# CLC-only figure. Potapov's "combined" is cropland-only (see above).
clc_nature_baseline_zone <- clc_change %>%
  filter(!clc_class %in% c('Marine inlets', 'Rivers and lakes')) %>%
  filter(!clc_class %in% c('Urban', 'Cropland')) %>%
  filter(!str_detect(clc_class, 'Gain')) %>%
  group_by(landwater) %>%
  summarise(totArea = sum(area, na.rm = TRUE)/1e6, .groups = 'drop')

ratio_to_inland <- plot_data %>%
  group_by(dataset, landwater) %>%
  summarise(netChange = sum(netChange, na.rm = TRUE), .groups = 'drop') %>%
  left_join(clc_nature_baseline_zone, by = 'landwater') %>%
  mutate(netChange_intensity = netChange / totArea * 100) %>%
  dplyr::select(dataset, landwater, netChange_intensity) %>%
  pivot_wider(names_from = landwater, values_from = netChange_intensity) %>%
  mutate(Coastal = abs(Coastal) / abs(Inland),
         Riparian = abs(Riparian) / abs(Inland)) %>%
  dplyr::select(dataset, Coastal, Riparian) %>%
  pivot_longer(c(Coastal, Riparian), names_to = 'landwater', values_to = 'ratio')

panel_c <- ratio_to_inland %>%
  ggplot(aes(x = dataset, y = ratio, color = landwater)) +
  geom_hline(yintercept = 1, linetype = 2, color = 'grey60') +
  geom_point(size = 3, position = position_dodge(width = 0.4)) +
  scale_color_manual(values = c('Coastal' = '#4582bb', 'Riparian' = '#62c8ca')) +
  labs(x = NULL,
       y = "Net loss intensity ratio\n(zone : inland)",
       color = 'Zone',
       title = 'c') +
  theme(legend.position = 'bottom')

figS_multidataset <- grid.arrange(panel_a, panel_b, panel_c, ncol = 3, padding = unit(0, "line"), newpage = F)

ggsave(out_path("figS_multidataset.png"), figS_multidataset, width = 34, height = 14, units = 'cm')

#### Class-definition crosswalk table (Table S3) -------------------------------
# Formal, citation-backed comparison of how each dataset defines "urban" and
# "cropland" (the two drivers this study tracks), building on the existing
# CLC/MAES crosswalk in Table S1. Sourced from the primary literature in
# data/literature/ (Maes et al. 2013; Ivits et al. 2024; Potapov et al. 2022;
# Zhang et al. 2024; CLMS CLC2018 Validation Report), not from the dataset
# providers' web pages - see the revision plan for the specific passages
# used. Descriptive only (see that plan for why a quantitative agreement
# check was left out of scope).
table_S3 <- tribble(
  ~Dataset, ~Variable, ~`Source classes`, ~`Spatial resolution`, ~Definition,

  "CLC (MAES)", "Urban",
    "CLC Level 3 codes 111, 112, 121-124, 131-133, 141, 142",
    "100 m (25 ha minimum mapping unit for the status layers; 5 ha minimum mapping unit and 100 m minimum mapping width for the change layer)",
    "Sealed surfaces, industrial/commercial/transport units, mineral extraction and construction sites, and artificial vegetated areas, reclassified to the MAES \"Urban\" ecosystem type (Table S1). Status compared between the 2000 and 2018 reference years (Maes et al. 2013; Ivits et al. 2024; CLMS CLC2018 Validation Report).",

  "CLC (MAES)", "Cropland",
    "CLC Level 3 codes 211-213, 221-223, 241-244",
    "100 m (as above)",
    "Arable land under crop rotation, permanent crops, and heterogeneous agricultural areas with a significant admixture of natural vegetation, reclassified to the MAES \"Cropland\" ecosystem type (Table S1). Status compared between the 2000 and 2018 reference years (Maes et al. 2013; Ivits et al. 2024; CLMS CLC2018 Validation Report).",

  "Potapov and GLAD built-up expansion", "Urban",
    "GLCLU2020 v2 LCLUC_2000 (baseline, value 250) vs. GLCLU2020 Builtup_type, value 2 (endpoint, built-up expansion 2000-2020)",
    "30 m (Landsat)",
    "Built-up land is defined as pixels that include man-made surfaces associated with infrastructure, commercial, or residential land use, even where such surfaces do not dominate the pixel. Built-up loss is not mapped by this product (Potapov et al. 2022). 2000 baseline nature/urban status taken from a separate, full-classification GLAD product (GLCLU2020 v2's annual layers), since Builtup_type alone carries no baseline classification of its own.",

  "Potapov and GLAD cropland", "Cropland",
    "GLCLU2020 v2 LCLUC_2000 (baseline, value 244) vs. Global cropland extent product, value 1 (endpoint, 2019)",
    "30 m (Landsat)",
    "Land used to produce annual or perennial herbaceous crops for human consumption, forage, or biofuel, with a fallow period of up to four years; excludes tree crops, permanent pastures, and shifting cultivation (Potapov et al. 2022). Compared between GLCLU2020 v2's 2000 baseline classification and the 2019 cropland layer, the nearest available year to this study's 2018 reference year.",

  "GLC-FCS30D", "Urban",
    "Impervious surface, code 190",
    "30 m (Landsat)",
    "Continuous-change-detection land-cover classification overlaid with an independently produced global impervious-surface-dynamics dataset (GISD30), validated to 90.1% overall accuracy (Zhang et al. 2024). Compared between the 2000 and 2018 annual layers.",

  "GLC-FCS30D", "Cropland",
    "Rainfed cropland (10), herbaceous cover cropland (11), tree or shrub cover cropland (12), irrigated cropland (20)",
    "30 m (Landsat)",
    "Four cropland subclasses of GLC-FCS30D's 35-class fine classification system, aggregated to a single cropland category for comparability with this study's CLC-derived definition (Zhang et al. 2024). Compared between the 2000 and 2018 annual layers."
)
write_csv(table_S3, out_path('table_S3_class_crosswalk.csv'))
