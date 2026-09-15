#### 00 - Setup: packages, config, and shared data import ------------------
#
# Sourced by every other script in this folder. Loads the packages, reads
# DATA_DIR/OUTPUTS_DIR from .env, and builds every table that more than one
# figure/table script depends on: the reporting grid, country/ecoregion
# lookups, the CLC change/status tables at grid, country and level-3 detail,
# the distance-band table, and the two rasters used for Figure 1's maps.
#
# See README.md for the full pipeline (which GEE exports feed which of the
# read_csv()/raster() calls below) and the required package list.
#
# Author: Zander Venter

library(raster)
library(tidyverse)
library(sf)
library(stringr)
library(gridExtra)
library(grid)
library(ggtext)
library(rnaturalearth)
library(pals)
library(khroma)

# Packages only needed by one specific script are loaded there instead of
# here, so the rest of the pipeline still runs if one of them is missing:
#   - ggmagnify, ggfx: R/01_figure1_overview.R only (ggfx is ggmagnify's
#     shadow-rendering dependency)
#   - gt, gtExtras: R/02_table1_accounting.R's optional interactive() block only
# ggpubr was dropped entirely - see R/01_figure1_overview.R's comment on why.

batlow <- color("batlow")
bam <- color("bam")

#### Config: DATA_DIR / OUTPUTS_DIR from .env ------------------------------
# .env lives at the repo root (gitignored). Falls back to ./data and
# ./outputs (relative to the working directory) if .env isn't found, so the
# scripts still work if someone clones the repo and drops data in place
# locally instead of using the shared drive locations.
# Assumes the working directory is the repo root (e.g. Rscript R/01_....R
# run from the repo root, or the RStudio project opened at the repo root).
if (file.exists(".env")) readRenviron(".env")

DATA_DIR <- Sys.getenv("DATA_DIR", unset = "./data")
OUTPUTS_DIR <- Sys.getenv("OUTPUTS_DIR", unset = "./outputs")
if (!dir.exists(OUTPUTS_DIR)) dir.create(OUTPUTS_DIR, recursive = TRUE)

data_path <- function(...) file.path(DATA_DIR, ...)
out_path <- function(...) file.path(OUTPUTS_DIR, ...)

#### Import and prepare datasets ---------------------------------------------------------------
grid <- st_read(data_path('export_grid_50km_landwater.shp')) %>% st_transform(st_crs(3035))

bbox_poly <- st_as_sfc(st_bbox(grid), crs = st_crs(grid))

countries <- ne_countries(scale = "medium", returnclass = "sf")%>% st_transform(st_crs(3035)) %>%
  st_filter(grid%>%st_union() %>% st_buffer(-100000)) %>%
  st_intersection(grid%>%st_union())
#countries <- st_read(data_path('countries.geojson'))%>% st_transform(st_crs(3035))

countriesShrunk <- countries %>%
  st_intersection(countries%>%st_union() %>% st_buffer(25000)%>% st_buffer(-40000))

ecoregions <- st_read(data_path('Ecoregions2017.shp')) %>%
  st_transform(st_crs(grid)) %>%
  st_filter(grid) %>%
  dplyr::select(BIOME_NAME, ECO_NAME)%>%
  mutate(BIOME_NAME = ifelse(BIOME_NAME == 'Temperate Grasslands, Savannas & Shrublands',
                             'Temperate Broadleaf & Mixed Forests', BIOME_NAME))%>%
  mutate(BIOME_NAME = ifelse(BIOME_NAME == 'Mediterranean Forests, Woodlands & Scrub',
                             'Mediterranean Woodlands & Scrub', BIOME_NAME))

# One dominant biome/ecoregion per grid cell (largest-overlap rule)
ecoregion_lookup <- grid %>%
  st_intersection(ecoregions) %>%
  mutate(area = as.numeric(st_area(geometry)) ) %>%
  st_drop_geometry() %>%
  group_by(id) %>%
  mutate(areaMax= max(area)) %>%
  filter(area == areaMax) %>%
  dplyr::select(id, BIOME_NAME, ECO_NAME)

clcLookup <- read_csv(data_path('clc_lookup.csv')) %>%
  dplyr::select(clc_class = `Map value`,
                clc_1 = `CLC level 1`,
                clc_3 = `CLC level 3`,
                maes = `MAES ecosystem type`)

# Detailed level-3 CLC land cover areas for baseline and end-point in change pixels
# (grid-level; from 03_areas_extract_grid.js). Feeds Figure 5.
# stratum encoding: landwater = stratum %/% 524, clc_class = stratum %% 524
# (baseMultiplier 524 > max CLC level-3 code, see getStratAreas() in the GEE scripts)
clc_change_l3 <- read_csv(data_path('from_gee/clc_areas_change_from_to_l3_2000_2018_grid_50km.csv'))%>%
  mutate(
    landwater   = stratum %/% 524,
    clc_class = (stratum %% 524)
  ) %>%
  filter(!change_code %in% c(14,15)) %>%
  mutate(change_code = recode_factor(factor(change_code),
                                   "10" = "Loss to urban",
                                   "11" = "Gain from urban",
                                   "12" = "Loss to cropland",
                                   "13" = "Gain from cropland"),
         landwater = recode_factor(factor(landwater),
                                   "1" = "Inland",
                                   "2" = "Coastal",
                                   "3" = "Riparian")) %>%
  left_join(clcLookup)%>%
  mutate(clc_base = str_split(change_code, ' ') %>% map_chr(3))%>%
  mutate(clc_lossgain = str_split(change_code, ' ') %>% map_chr(1))

# Change areas for simplified typology including baseline areas (grid-level;
# from 03_areas_extract_grid.js). Feeds Figures 1, 3, 4, S2 and the biome/
# ecoregion summaries.
# stratum encoding: landwater = stratum %/% 14, clc_class = stratum %% 14
# (baseMultiplier 14 > max class code: 9 MAES classes + 4 change codes)
clc_change <- read_csv(data_path('from_gee/clc_areas_change_2000_2018_grid_50km.csv'))%>%
  mutate(
    landwater   = stratum %/% 14, # see getStratAreas() function in GEE code baseMultiplier
    clc_class = (stratum %% 14)
  ) %>%
  # Small area in CLC over France/Andora (1.6738, 42.5632) has no data - exclude this
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
                                   "3" = "Riparian")) %>%
  left_join(ecoregion_lookup, by = 'id')

# Same as clc_change above, but aggregated to country instead of grid cell
# (from 04_areas_extract_countries.js). Feeds Table 1 / Table S2 (via
# 02_table1_accounting.R) and Figure 2c/d.
clc_change_country <-  read_csv(data_path('from_gee/clc_areas_change_2000_2018_countries.csv'))%>%
  mutate(
    landwater   = stratum %/% 14,
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
                                   "3" = "Riparian")) %>%
  mutate(area = area/1000000)

# Baseline (2000) urban/cropland/nature area per country (from
# 04_areas_extract_countries.js). Opening stock for the Table 1/S2 accounts.
# stratum encoding: landwater = stratum %/% 4, lc = stratum %% 4
clc_base_simp <- read_csv(data_path('from_gee/clc_areas_simp_baseline_countries.csv'))%>%
  mutate(
    landwater   = stratum %/% 4,
    lc = (stratum %% 4)
  ) %>%
  # Filter out the zeros which are water/NA
  filter(lc != 0) %>%
  mutate(landwater = recode_factor(factor(landwater),
                                   "1" = "Inland",
                                   "2" = "Coastal",
                                   "3" = "Riparian"),
         lc = recode_factor(factor(lc),
                                        "1" = "urban",
                                        "2" = "cropland",
                                        "3" = "nature")) %>%
  dplyr::select(country, lc, areaBase=area, landwater)%>%
  mutate(areaBase = areaBase/1000000)

# Change areas within concentric distance bands of the water's edge
# (grid-level; from 03_areas_extract_grid.js). Feeds Figure 3 and Figure S1.
clc_change_distance <- read_csv(data_path('from_gee/clc_areas_change_distance_2000_2018_grid_50km.csv')) %>%
  filter(!stratum %in% c(0)) %>%
  mutate(stratum = recode_factor(factor(stratum),
                                 "1" = "nature",
                                 "2" = "cropland",
                                 "3" = "urban",
                                 "4" = "urban -> nature",
                                 "5" = "cropland -> nature",
                                 "6" = "nature -> urban",
                                 "7" = "nature -> cropland")) %>%
  mutate(type = ifelse(type == 'all', 'all water',
                       ifelse(type == 'coastal', 'coastal zone', 'riparian zone'))) %>%
  dplyr::select(id, type,stratum, distance, area) %>%
  mutate(distance = distance/1000)

# Raster images for visualization in R (Figure 1a/b)
clcChange_rast <- raster(data_path('from_gee/clc18_maes_change_500m.tif'))
clcChange_rast[clcChange_rast == 0] <- NA
clcChange_rast_1000m <- aggregate(clcChange_rast, fact = 3, fun = max, na.rm = TRUE)

landwater_rast <- raster(data_path('from_gee/landwater_500m.tif'))
landwater_rast[landwater_rast == 0] <- NA
# Relabel from the export encoding (1 inland, 2 coastal, 3 riparian) to the
# plotting order used in Figure 1a (Inland, Riparian, Coastal)
landwater_rast[landwater_rast == 2] <- 99
landwater_rast[landwater_rast == 3] <- 2
landwater_rast[landwater_rast == 99] <- 3
landwater_rast_1000m <- aggregate(landwater_rast, fact = 3, fun = max, na.rm = TRUE)
