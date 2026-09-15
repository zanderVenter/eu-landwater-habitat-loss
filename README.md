# eu-landwater-habitat-loss

Code supporting the manuscript **"The land-water interface is
disproportionately affected by habitat loss from land use"** (currently
under revision at *Nature Communications*). It quantifies loss and recovery
of (semi-)natural habitat from urban and cropland expansion/abandonment
within coastal and riparian zones across the EEA-39 countries, 2000-2018,
using the Corine Land Cover (CLC) Accounting Layers.

**Nothing in this pipeline is automated end-to-end.** It has three manual
stages: run Google Earth Engine (GEE) scripts in the Code Editor, download
their exports from Google Drive by hand, then run the R analysis locally.
This document walks through all three.

## Data provenance

- **Corine Land Cover Accounting Layers (CLC AL)** (Ivits et al., 2024) -
  temporally harmonised land-cover status/change for 2000-2018, EEA-39,
  100m resolution.
- **Copernicus Riparian Zones** and **Copernicus Coastal Zones** (CLMS) -
  define the land-water interface (riparian: Strahler-order-dependent
  buffers around EU-HYDRO rivers/lakes; coastal: buffered EU-HYDRO
  coastline, clipped to 1500m inland here to match the max riparian
  buffer).
- **RESOLVE Ecoregions 2017** - biome/ecoregion stratification (Figure 4).
- **GISCO country boundaries** and **Natural Earth** - country polygons for
  the country-level accounting (Table 1, S2, Figure 2) and basemaps.

See `manuscript/supplement.docx` (gitignored, local only) Table S1 for the
full CLC-to-MAES ecosystem typology crosswalk.

## Pipeline overview

```
GEE Code Editor (manual)          Google Drive        DATA_DIR (manual copy)      R (local)
  01_strata_img_generate.js   -->  EE Assets      \
  02_distance_img_generate.js -->  EE Assets       \--> read by 03 & 04 below
  03_areas_extract_grid.js    -->  CSVs/TIFs   -->  download  -->  data/from_gee/  -->  R/00-07
  04_areas_extract_countries.js -> CSVs        -->  download  -->  data/from_gee/
```

Scripts 01 and 02 export image assets *within Earth Engine* (no download
needed there); scripts 03 and 04 export CSVs/GeoTIFFs *to Google Drive*,
which you then download by hand into `DATA_DIR`.

## Step 1 - Earth Engine (manual, Code Editor)

Google Earth Engine JS scripts are not run from the command line - open
[code.earthengine.google.com](https://code.earthengine.google.com), paste
each script's contents into a new script tab, and click Run.

**Ask before running any of these** - see `CLAUDE.md`. Full continental
exports consume meaningful EECU budget.

Run in this order:

1. **`01_strata_img_generate.js`** - builds the coastal/riparian/inland
   strata image and exports it, one tile per EEA-39 grid cell, to
   `projects/gee-zander-nina/assets/Arena/strataImg_riparian_coastal`.
   Has a `maxCellsToExport` variable at the top of the export section,
   committed as `2` (a cheap smoke test) - set it to the grid's full length
   for a production run.
2. **`02_distance_img_generate.js`** - same pattern, exports distance-to-
   coast/distance-to-freshwater images to
   `projects/gee-zander-nina/assets/Arena/distImg_riparian_coastal`. Also
   has a `maxCellsToExport` toggle.
3. **`03_areas_extract_grid.js`** - mosaics 01 and 02's outputs, derives
   the land-cover change layers, and exports (to Google Drive):
   `landwater_500m.tif`, `clc18_maes_1000m.tif`, `clc18_maes_change_500m.tif`,
   `clc_areas_change_2000_2018_grid_50km.csv`,
   `clc_areas_change_from_to_l3_2000_2018_grid_50km.csv`,
   `clc_areas_change_distance_2000_2018_grid_50km.csv`.
4. **`04_areas_extract_countries.js`** - same change-layer derivation,
   aggregated to countries instead of the 50km grid. Exports (to Google
   Drive): `clc_areas_change_2000_2018_countries.csv`,
   `clc_areas_simp_baseline_countries.csv`,
   `clc_areas_consumption_formation_2000_2018_countries.csv` (this last one
   isn't read by the current R pipeline - see the file's header comment).

### Asset path provenance (why there are three different prefixes)

- `projects/nina/...` - legacy NINA-org GEE assets (CLC layers, CLMS
  riparian/coastal source polygons, the EEA-39 tiling grid).
- `projects/gee-zander-nina/assets/Arena/...` - current project assets
  (the strata/distance image collections this pipeline produces and
  consumes).
- `users/zandersamuel/Global_misc/...` - a personal-namespace GEE asset
  (the GISCO country boundaries used in `04`).

None of these are the same as the `GEE_PROJECT` value in `.env`, which is
the Cloud project Earth Engine bills export tasks against - distinct from
which project *hosts* an asset you're reading.

## Step 2 - manual download

GEE `Export.table.toDrive`/`Export.image.toDrive` tasks land in **your**
Google Drive, not on this machine. After each task finishes (Earth Engine
Code Editor's Tasks tab), download the files and place them here, relative
to `DATA_DIR` (see `.env`):

| File | Destination |
|---|---|
| `landwater_500m.tif` | `data/from_gee/landwater_500m.tif` |
| `clc18_maes_1000m.tif` | `data/from_gee/clc18_maes_1000m.tif` |
| `clc18_maes_change_500m.tif` | `data/from_gee/clc18_maes_change_500m.tif` |
| `clc_areas_change_2000_2018_grid_50km.csv` | `data/from_gee/clc_areas_change_2000_2018_grid_50km.csv` |
| `clc_areas_change_from_to_l3_2000_2018_grid_50km.csv` | `data/from_gee/clc_areas_change_from_to_l3_2000_2018_grid_50km.csv` |
| `clc_areas_change_distance_2000_2018_grid_50km.csv` | `data/from_gee/clc_areas_change_distance_2000_2018_grid_50km.csv` |
| `clc_areas_change_2000_2018_countries.csv` | `data/from_gee/clc_areas_change_2000_2018_countries.csv` |
| `clc_areas_simp_baseline_countries.csv` | `data/from_gee/clc_areas_simp_baseline_countries.csv` |
| `clc_areas_consumption_formation_2000_2018_countries.csv` | `data/from_gee/clc_areas_consumption_formation_2000_2018_countries.csv` (optional, not read by R) |

`DATA_DIR` (see `.env`) is already fully populated on the shared drive, so
this step is only needed if you re-run the GEE scripts and want to refresh
the inputs. `DATA_DIR` also needs, independent of the GEE exports above
(already present, static reference data - not regenerated by any script
here): `export_grid_50km_landwater.{shp,shx,dbf,prj}`, `Ecoregions2017.shp`
(+ sidecar files), `clc_lookup.csv`, and `png/*.png` (the Figure 1
before/after example image pairs).

## Step 3 - R environment

```r
install.packages(c(
  "raster", "tidyverse", "sf", "gridExtra", "ggtext",
  "rnaturalearth", "pals", "khroma", "gt", "gtExtras", "ggfx"
))
# GitHub-only package:
remotes::install_github("hughjonesd/ggmagnify")
```

The original script used `ggpubr::background_image()`/`ggarrange()` in
`R/01_figure1_overview.R`, but `ggpubr` pulls in a large `car`/`rstatix`/
`lme4` dependency chain (which on Linux needs the system library
`libnlopt`, e.g. `sudo apt-get install libnlopt-dev`) for two functions
with trivial equivalents already used elsewhere in this pipeline -
`ggplot2::annotation_raster()` and `gridExtra::grid.arrange()`. `ggpubr` has
been dropped entirely rather than worked around; see the comment at the top
of `R/01_figure1_overview.R`. `ggfx` is `ggmagnify`'s (optional) dependency
for its `shadow=TRUE` rendering, used in that same script.

Copy `.env` (see the template already in this repo) and confirm
`DATA_DIR`/`OUTPUTS_DIR` point at the right places for your machine.

## Step 4 - run the R pipeline

Run from the repo root (the scripts assume that working directory, and read
`.env` from there):

```bash
Rscript R/run_all.R
```

...or run any individual script directly (each sources its own
prerequisites, so running e.g. `R/06_figure5_l3_drivers.R` alone re-sources
`R/00_setup.R` first without needing you to run the others):

| Script | Manuscript output | Depends on |
|---|---|---|
| `R/00_setup.R` | (shared data prep, not run directly) | - |
| `R/01_figure1_overview.R` | Figure 1 | `00` |
| `R/02_table1_accounting.R` | Table 1, Table S2 | `00` |
| `R/03_figure2_country_maps.R` | Figure 2 | `02` (which sources `00`) |
| `R/04_figure3_distance_gradients.R` | Figure 3, Figure S1 | `02` |
| `R/05_figure4_biome_panels.R` | Figure 4 | `00` |
| `R/06_figure5_l3_drivers.R` | Figure 5 | `00` |
| `R/07_figureS2_bivariate_maps.R` | Figure S2 | `00` |

All figures/tables are written to `OUTPUTS_DIR` (see `.env`), never into
the repo.

## Known gaps

- **Coastal buffer comment/code mismatch**: `01_strata_img_generate.js`'s
  comment says the coastal zone is buffered "1500m inland and 100m into
  water", but the water-side buffer line is commented out in the code -
  only the 1500m inland buffer is actually applied. Flagged inline with a
  `TODO(zander)` rather than silently changed either way - needs your
  confirmation of which was actually used for the submitted results.
- **`clc_areas_consumption_formation_2000_2018_countries` export**: correctly
  implemented in `04_areas_extract_countries.js` and downloadable, but not
  currently read by any R script - the accounting table derives
  consumption/formation directly from `clc_areas_change_2000_2018_countries.csv`
  instead. Kept as an independent cross-check, not a pipeline dependency.
- **GLAD data & `legacy.R`**: not part of the current pipeline (see
  `CLAUDE.md`) - earmarked for a possible response to a reviewer's
  resolution-sensitivity concern (CLC's 100m MMU vs. finer products for
  narrow riparian corridors) in the next revision pass, not dead code.
- **`maxCellsToExport` smoke-test default**: `01`/`02`'s committed default
  only exports 2 grid cells - a deliberate cheap test, not a full run. See
  Step 1 above.

## Verification

All of `R/01`-`R/07` have been run end-to-end via `Rscript` against the
data already in `DATA_DIR`, with no errors, and spot-checked against the
manuscript text:

- `R/02_table1_accounting.R`'s regenerated `change_account_eu.csv`
  reproduces the manuscript's headline figures exactly: 16,337.35 km² gross
  consumption, 8,910.17 km² gross formation, 7,427.18 km² net loss
  (manuscript: 16,337 / 8,910 / 7,427 km²).
- `R/01_figure1_overview.R`'s regenerated `fig1.png` panel (c) reproduces
  the manuscript's baseline land-cover shares exactly: urban 4.3%, cropland
  35.4% (manuscript: "Urban areas covered only 4.3%... compared to 35.4%
  covered by cropland").
- `R/03_figure2_country_maps.R`'s regenerated `fig2.png` reproduces the
  manuscript's country rankings exactly: Portugal/Belgium/Serbia rank
  highest on the land-water:inland net-loss ratio (manuscript: "the
  highest relative intensities recorded in Portugal, Belgium, and
  Serbia"), and Belgium/Cyprus/Romania/Albania rank highest on net loss
  intensity at the land-water interface, all close to 1% (manuscript:
  "Belgium, Cyprus, Romania and Albania exhibited the greatest net habitat
  loss intensities at the land-water interface, close to 1%").
