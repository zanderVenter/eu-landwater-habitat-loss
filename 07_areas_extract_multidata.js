/**
 * 07 - Extract comparable nature-loss/gain areas from three independent
 * land-cover datasets (CLC, Potapov/GLAD, GLC-FCS30D), crossed with the
 * coastal/riparian/inland strata
 *
 * Revision addition (response to Reviewer 2's concern that CLC's 100m MMU
 * may miss land-use changes in narrow riparian corridors). Derives
 * comparable urban/cropland-driven nature-loss and nature-gain binary
 * layers from three 30m-class, Landsat-derived products and cross-tabs
 * each against the existing landwater strata, the same way CLC's own
 * change layers are cross-tabbed everywhere else in this pipeline. Feeds
 * R/10_figureS_multidataset.R (per-year net-change-by-dataset comparison
 * figure) and the written class-definition crosswalk table.
 *
 * IMPORTANT - every dataset's 2000 baseline nature/urban/cropland check
 * below uses that dataset's OWN 2000 classification, not CLC's - see
 * section 6 for why. (An earlier version of this script anchored every
 * dataset to CLC's 2000 baseline instead; reverted because Potapov's own
 * baseline classification is available - see GLCLU2020 v2 LCLUC_2000
 * below - and a dataset's own change signal is more directly comparable to
 * its own baseline than to a different product's classification. GLC-FCS30D
 * already used its own baseline throughout; Potapov built-up/cropland did
 * not, because Builtup_type/Global_cropland_YYYY carry no baseline
 * classification of their own - fixed below by sourcing Potapov's 2000
 * baseline from a separate, full-classification GLAD product instead.)
 * The three datasets still differ in observation window and endpoint
 * source, and that's deliberately left visible rather than papered over
 * (see the written discussion this feeds):
 *   - CLC: true 2000 vs. 2018 status comparison, both directions (loss and
 *     gain) for both urban and cropland.
 *   - Potapov built-up (GLAD): baseline from GLCLU2020 v2 LCLUC_2000;
 *     endpoint is only an expansion flag for 2000-2020 (no
 *     "abandonment"/contraction signal at all) - gain_from_urban is not
 *     derivable from this dataset.
 *   - Potapov cropland (GLAD): baseline from GLCLU2020 v2 LCLUC_2000;
 *     endpoint is the 2019 snapshot (nearest available to CLC's 2018), not
 *     an exact-year match.
 *   - GLC-FCS30D: true annual time series, b1=2000 and b19=2018 used here
 *     for an exact-year match to CLC.
 *
 * Inputs (Earth Engine assets, must already exist):
 *   - projects/nina/Europe_misc/CLMS_CLCplus_RASTER_2018_010m_eu_03035_V1_1
 *   - projects/gee-zander-nina/assets/Arena/strataImg_riparian_coastal (from 01)
 *   - projects/nina/Europe_misc/CLC2000ACC_V2018_20, CLC2018ACC_V2018_20
 *   - projects/glad/GLCLU2020/v2/LCLUC_2000 (Potapov et al. 2022; full
 *     2000 land-cover/land-use classification, used ONLY to derive
 *     Potapov's own baseline nature/urban/cropland masks - legend at
 *     https://storage.googleapis.com/earthenginepartners-hansen/GLCLU2000-2020/v2/legend.xlsx,
 *     "Annual" sheet: 0-48 Terra Firma, 100-148 Wetland, 200-207 open
 *     water, 241 snow/ice, 244 Cropland, 250 Built-up, 254 Ocean)
 *   - projects/glad/OceanMask (land/ocean mask for the LCLUC product)
 *   - projects/glad/GLCLU2020/Builtup_type (Potapov et al. 2022,
 *     https://doi.org/10.3389/frsen.2022.856903; 1=stable built-up,
 *     2=built-up expansion 2000-2020)
 *   - users/potapovpeter/Global_cropland_2019 (Potapov et al. 2022,
 *     https://doi.org/10.1038/s43016-021-00429-z; 0=no cropland,
 *     1=cropland)
 *   - projects/sat-io/open-datasets/GLC-FCS30D/annual (Zhang et al. 2024,
 *     https://doi.org/10.5194/essd-16-1353-2024; b1=2000...b23=2022)
 *
 * NOTE ON ASSET TYPES (confirmed by Zander): Builtup_type is a plain
 * pre-mosaicked ee.Image, loaded directly. The Global_cropland_YYYY assets
 * and GLC-FCS30D's annual collection are per-tile ee.ImageCollections
 * (the latter confirmed by the _glc_fcs30d.js reference script's
 * .mosaic() calls) and are mosaicked below.
 *
 * Output (Google Drive, one task per country - see section 9 below for why -
 * download the whole "multidata_countries" Drive folder into
 * data/from_gee/multidata_countries/):
 *   - clc_areas_change_multidata_<ISO3>.csv per country, each with one row
 *     per landwater zone x dataset x change_type (loss_to_urban/
 *     gain_from_urban/loss_to_cropland/gain_from_cropland, where
 *     applicable per dataset - see the asymmetry note above). stratum
 *     encoding: landwater*2 + flag (binary layer, baseMultiplier=2, same
 *     trick as the consumption/formation export in
 *     04_areas_extract_countries.js). R reads the whole folder with
 *     list.files(), same pattern already used for
 *     clc_areas_consumption_formation_2000_2018_countries/.
 *
 * Author: Zander Venter
 */


/***
 * 1. Shared setup (strata - same as 03/04/05/06/08) ----------------------------
 */
var projCrs = 'EPSG:3035'
var proj = ee.Projection(projCrs);

var clcplus = ee.Image('projects/nina/Europe_misc/CLMS_CLCplus_RASTER_2018_010m_eu_03035_V1_1');
var landMask = clcplus.neq(10).and(clcplus.neq(254))

var strataImgCol = ee.ImageCollection('projects/gee-zander-nina/assets/Arena/strataImg_riparian_coastal');
var strataImg_landwater = strataImgCol.mosaic();
strataImg_landwater = strataImg_landwater.unmask(1).updateMask(landMask);


/***
 * 2. CLC change layers (same crosswalk as 03/04/05/06/08) ---------------------
 */
var clc00_raw = ee.Image('projects/nina/Europe_misc/CLC2000ACC_V2018_20');
var clc18_raw = ee.Image('projects/nina/Europe_misc/CLC2018ACC_V2018_20');

var clcVals = [
  111,112,121,122,123,124,131,132,133,141,142, // Urban
  211,212,213,221,222,223,                     // Cropland
  231,                                         // Grassland
  241,242,243,244,                             // Cropland
  311,312,313,                                 // Woodland and forest
  321,                                         // Grassland
  322,323,                                     // Heathland and shrub
  324,                                         // Woodland and forest
  331,332,333,334,335,                         // Sparsely vegetated land
  411,412,                                     // Wetlands
  421,422,423,                                 // Marine inlets and transitional waters
  511,512,                                     // Rivers and lakes
  521,522,523                                  // Marine inlets and transitional waters
];
var maesVals = [
  1,1,1,1,1,1,1,1,1,1,1, // Urban
  2,2,2,2,2,2,           // Cropland
  3,                     // Grassland
  2,2,2,2,               // Cropland
  4,4,4,                 // Woodland and forest
  3,                     // Grassland
  5,5,                   // Heathland and shrub
  4,                     // Woodland and forest
  6,6,6,6,6,             // Sparsely vegetated land
  7,7,                   // Wetlands
  8,8,8,                 // Marine inlets and transitional waters
  9,9,                   // Rivers and lakes
  8,8,8                  // Marine inlets and transitional waters
];

var clc00 = clc00_raw.remap(clcVals, maesVals, 0);
var clc18 = clc18_raw.remap(clcVals, maesVals, 0);
// "Nature" = anything terrestrial and not urban/cropland (excludes water/
// marine classes 8-9, same as everywhere else in this pipeline)
var clcNature00 = clc00.gt(2).and(clc00.lt(8));
var clcNature18 = clc18.gt(2).and(clc18.lt(8));
var clcUrban00 = clc00.eq(1); var clcUrban18 = clc18.eq(1);
var clcCrop00 = clc00.eq(2); var clcCrop18 = clc18.eq(2);


/***
 * 3. Potapov/GLAD baseline (2000) classification + built-up endpoint ----------
 */
// Builtup_type carries no baseline land-cover information of its own (it's
// a 2-value stable/expansion flag, not a full 2000 classification), so
// without an external baseline check this would count ANY expansion onto
// ANY prior land cover - including cropland -> urban - as if it were
// nature -> urban. Fixed by sourcing Potapov's own 2000 baseline from a
// separate, full-classification GLAD product (GLCLU2020 v2's annual
// layers) instead - same lab/sensor lineage as Builtup_type and the
// cropland layers, so this stays a self-referential Potapov baseline, not
// a cross-dataset one.
var potapovLandmask = ee.Image('projects/glad/OceanMask').lte(1);
var potapovBaseline00 = ee.Image('projects/glad/GLCLU2020/v2/LCLUC_2000').updateMask(potapovLandmask);
// Annual-layer legend (see file header for the source): Built-up = 250,
// Cropland = 244, Nature = Terra Firma (0-48) + Wetland (100-148) +
// Snow/ice (241) - excludes open water (200-207), ocean (254) and no-data
// (255), and mirrors CLC's own nature definition (which also excludes
// urban/cropland/water/marine and keeps glaciers/perpetual snow under
// "sparsely vegetated").
var potapovUrban00 = potapovBaseline00.eq(250);
var potapovCrop00 = potapovBaseline00.eq(244);
var potapovNature00 = potapovBaseline00.gte(0).and(potapovBaseline00.lte(48))
  .or(potapovBaseline00.gte(100).and(potapovBaseline00.lte(148)))
  .or(potapovBaseline00.eq(241));

var potapovBuiltup = ee.Image('projects/glad/GLCLU2020/Builtup_type');
// value 2 = built-up expansion 2000-2020. Gated on potapovNature00 (this
// section, above) below to restrict to nature -> urban; see section 6.


/***
 * 4. Potapov/GLAD cropland endpoint (2019 - nearest available to 2018) --------
 */
// Only the 2019 layer is needed: baseline comes from potapovNature00/
// potapovCrop00 (section 3, sourced from GLCLU2020 v2 LCLUC_2000), not
// from Potapov's own 2003 cropland snapshot. Global_cropland_YYYY only
// tells us cropland/not-cropland, not what "not cropland" actually is - so
// gain_from_cropland, if gated on a Potapov cropland layer alone, would
// count cropland -> urban as if it were cropland -> nature (recovery),
// when it's really further habitat loss. Gating on potapovNature00/
// potapovCrop00 instead (section 6) fixes that, the same way as built-up
// above.
var potapovCrop19 = ee.ImageCollection('users/potapovpeter/Global_cropland_2019').mosaic();


/***
 * 5. GLC-FCS30D (true 2000 vs. 2018 annual bands) ------------------------------
 */
var glcAnnual = ee.ImageCollection('projects/sat-io/open-datasets/GLC-FCS30D/annual').mosaic();
var glc2000 = glcAnnual.select('b1');   // 1999 + 1
var glc2018 = glcAnnual.select('b19');  // 1999 + 19

// GLC-FCS30D class codes (see _glc_fcs30d.js reference script for the full
// 35-class legend): cropland = 10/11/12/20, impervious surfaces = 190,
// water body = 210, permanent ice/snow = 220.
var glcCropVals = [10, 11, 12, 20];
function glcIsCropland(img) {
  return img.eq(glcCropVals[0]).or(img.eq(glcCropVals[1]))
    .or(img.eq(glcCropVals[2])).or(img.eq(glcCropVals[3]));
}
var glcUrban00 = glc2000.eq(190); var glcUrban18 = glc2018.eq(190);
var glcCrop00 = glcIsCropland(glc2000); var glcCrop18 = glcIsCropland(glc2018);
// "Nature" = not cropland, not impervious (190), not water body (210), not
// permanent ice/snow (220) - the GLC-FCS30D equivalent of CLC's exclusion
// of its own urban/cropland/water/marine classes from clcNature.
function glcIsNature(img) {
  return glcIsCropland(img).not().and(img.neq(190)).and(img.neq(210)).and(img.neq(220));
}
var glcNature00 = glcIsNature(glc2000);
var glcNature18 = glcIsNature(glc2018);
// glcUrban00/glcCrop00/glcNature00 (GLC-FCS30D's own 2000 status) are used
// as the baseline gate in section 6, below - GLC-FCS30D is a true annual
// time series, so unlike Potapov it already has its own baseline
// classification at the same 30m resolution as its endpoint, no separate
// asset needed.


/***
 * 6. Assemble a labelled list of binary change layers --------------------------
 * Every dataset's baseline (2000) and endpoint land-cover check below uses
 * that dataset's OWN classification - CLC vs. CLC, Potapov vs. Potapov
 * (baseline from GLCLU2020 v2 LCLUC_2000, section 3; endpoint from
 * Builtup_type/Global_cropland_2019), GLC-FCS30D vs. GLC-FCS30D. This is a
 * genuine three-way independent comparison: each dataset detects change
 * relative to its own understanding of what was "nature" in 2000, so
 * agreement or disagreement between datasets reflects real differences in
 * change-detection sensitivity (e.g. resolution, per Reviewer 2's
 * concern), not an artifact of forcing everything through one dataset's
 * baseline classification.
 *
 * Plain client-side array (not an ee.ImageCollection - we already know
 * exactly how many there are, and iterate it client-side below, so there's
 * no need for a server round-trip to count it).
 */
var changeLayers = [
  {image: ee.Image(0).where(clcNature00.and(clcUrban18), 1), dataset: 'clc', change_type: 'loss_to_urban'},
  {image: ee.Image(0).where(clcUrban00.and(clcNature18), 1), dataset: 'clc', change_type: 'gain_from_urban'},
  {image: ee.Image(0).where(clcNature00.and(clcCrop18), 1), dataset: 'clc', change_type: 'loss_to_cropland'},
  {image: ee.Image(0).where(clcCrop00.and(clcNature18), 1), dataset: 'clc', change_type: 'gain_from_cropland'},

  {image: ee.Image(0).where(potapovNature00.and(potapovBuiltup.eq(2)), 1), dataset: 'potapov', change_type: 'loss_to_urban'},
  // no potapov gain_from_urban - see file header
  {image: ee.Image(0).where(potapovNature00.and(potapovCrop19.eq(1)), 1), dataset: 'potapov', change_type: 'loss_to_cropland'},
  {image: ee.Image(0).where(potapovCrop00.and(potapovCrop19.eq(0)), 1), dataset: 'potapov', change_type: 'gain_from_cropland'},

  {image: ee.Image(0).where(glcNature00.and(glcUrban18), 1), dataset: 'glc_fcs30d', change_type: 'loss_to_urban'},
  {image: ee.Image(0).where(glcUrban00.and(glcNature18), 1), dataset: 'glc_fcs30d', change_type: 'gain_from_urban'},
  {image: ee.Image(0).where(glcNature00.and(glcCrop18), 1), dataset: 'glc_fcs30d', change_type: 'loss_to_cropland'},
  {image: ee.Image(0).where(glcCrop00.and(glcNature18), 1), dataset: 'glc_fcs30d', change_type: 'gain_from_cropland'}
];
print('number of change layers', changeLayers.length);


/***
 * 7. Area helper functions (binary layers, baseMultiplier=2) ------------------
 */
function getAreas(stratImage, aoi, scale, proj) {
  var strataAreas = ee.Image.pixelArea()
    .addBands(stratImage)
    .reproject(proj.atScale(scale))
    .reduceRegion({
      reducer: ee.Reducer.sum().group(1),
      geometry: aoi,
      scale: scale,
      maxPixels: 1e14,
      bestEffort: true
    });
  var groups = ee.List(strataAreas.get('groups'));
  var strataInfo = groups.map(function(group) {
    var dict = ee.Dictionary(group).rename(['sum', 'group'], ['area', 'stratum']);
    return ee.Feature(null, dict);
  });
  return ee.FeatureCollection(strataInfo);
}

function getStratAreas(stratImage, aoi, scale, proj, baseMultiplier) {
  var strataImg_landwater_reproj = strataImg_landwater.reproject(proj.atScale(scale));
  var combined = strataImg_landwater_reproj.multiply(ee.Image(baseMultiplier)).add(stratImage);
  return getAreas(combined, aoi, scale, proj);
}


/***
 * 8. Country boundaries (same as 04_areas_extract_countries.js) ---------------
 * At 30m, one combined grid.map().flatten() export covering all of Europe
 * (as 03/05/06/08 do at 100m) risks exactly the timeout/"computation graph
 * too large" failure you'd expect from ~3069 grid cells x 11 change layers
 * of 30m reduceRegion calls chained into a single task. Following the
 * per-country task-splitting pattern from your eu-dw-landtake project
 * (gee-scripts/5_strata_areas_extract.js, section 3) instead: one
 * reduceRegion per country per layer (a handful of calls), each country
 * exported as its own separate task, rather than one mega-task. Each
 * task's pixel count is also comfortably under maxPixels even for the
 * largest EEA-39 countries at 30m, so `bestEffort` never needs to
 * silently coarsen the scale (which would defeat the point of this
 * comparison) - see the file header for why 30m specifically matters here.
 */
var countries = ee.FeatureCollection('users/zandersamuel/Global_misc/GISCO_CNT_RG_01M_2024');

var selectedEEA = [
    "ALB","AUT","BEL","BIH","BGR","CZE","CYP","DEU","DNK","ESP","EST","FIN",
    "FRA","GBR","GRC","HRV","HUN","IRL","ITA","ISL","LIE","LTU","LUX","LVA",
    "MKD","MLT","MNE","NLD","NOR","POL","PRT","ROU","SRB","SVK","SVN","SWE",
    "TUR","XKX","CHE"]

// Same overseas-territory exclusion as 04_areas_extract_countries.js /
// 06_areas_extract_basins.js (French Guiana, Svalbard, etc. - outside the
// CLC Accounting Layers' coverage, would otherwise contribute garbage/zero
// pixels to a country's totals).
var excludeArea = ee.Geometry({
      "type": "GeometryCollection",
      "geometries": [
        {
          "type": "Polygon",
          "coordinates": [
            [
              [1.7527057855936556, 81.31775845273751],
              [3.3347370355936556, 73.74409530440026],
              [38.315205785593655, 72.88632759705337],
              [40.776143285593655, 79.53999751306542],
              [39.369893285593655, 81.31775845273751]
            ]
          ],
          "evenOdd": true
        },
        {
          "type": "Polygon",
          "coordinates": [
            [
              [-63.50510371029064, 24.63642145462176],
              [-74.75510371029064, 11.29627646126898],
              [-54.36447871029064, -2.1644626175286628],
              [-46.45432246029064, 0.12017854256283018],
              [-46.98166621029064, 6.609377574331028]
            ]
          ],
          "evenOdd": true
        },
        {
          "type": "Polygon",
          "coordinates": [
            [
              [42.83377898436505, -7.730745165510744],
              [42.83377898436505, -15.980493877672831],
              [56.19315398436504, -24.385270757831574],
              [59.88456023436504, -20.15718470553273]
            ]
          ],
          "geodesic": true,
          "evenOdd": true
        }
      ],
      "coordinates": []
    });
countries = countries.filter(ee.Filter.inList('ISO3_CODE', selectedEEA))
countries = countries.map(function(ft){return ft.difference(excludeArea)})


/***
 * 9. One export task per country -----------------------------------------------
 * Mirrors eu-dw-landtake's pattern: evaluate the country list client-side
 * (.evaluate()), then a plain for-loop that queues one Export.table.toDrive
 * task per country - each with its own small computation graph (11 layers
 * x 1 reduceRegion each), rather than nesting a country loop inside a
 * server-side .map(). All tasks land in the same Drive folder; combine
 * them in R the same way clc_areas_consumption_formation_2000_2018_countries/
 * is already read (list.files() over the folder).
 *
 * Set RUN_ALL_COUNTRIES to true once you've reviewed a couple of tasks and
 * are happy with them - it starts as false (2-country smoke test) for the
 * same reason 01/02's maxCellsToExport defaults small: ask before scaling
 * up to the full 38-country run.
 */
var RUN_ALL_COUNTRIES = false;

countries.aggregate_array('ISO3_CODE').evaluate(function(isoList) {
  print('countries', isoList);

  var nCountries = RUN_ALL_COUNTRIES ? isoList.length : Math.min(2, isoList.length);
  for (var i = 0; i < nCountries; i++) {
    var iso = isoList[i];
    var aoi = countries.filter(ee.Filter.eq('ISO3_CODE', iso)).geometry();

    var countryAreas = changeLayers.map(function(layer) {
      var stratAreas = getStratAreas(layer.image, aoi, 30, clc00.projection(), 2);
      stratAreas = stratAreas.map(function(f) {
        return f.set('country', iso, 'dataset', layer.dataset, 'change_type', layer.change_type)
      })
      return stratAreas
    });
    countryAreas = ee.FeatureCollection(countryAreas).flatten();

    Export.table.toDrive({
      collection: countryAreas,
      fileFormat: 'CSV',
      description: 'clc_areas_change_multidata_' + iso,
      folder: 'multidata_countries'
    })
  }
})
