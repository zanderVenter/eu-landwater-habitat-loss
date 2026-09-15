/**
 * 04 - Extract land-cover change and baseline areas aggregated to country
 *
 * Pipeline step 4 of 4 (Google Earth Engine, run manually in the Code
 * Editor, after 01 and 02 have finished exporting their asset collections).
 * See README.md for the full pipeline order.
 *
 * This is the country-level counterpart to 03_areas_extract_grid.js -
 * same change-layer derivation, aggregated to countries instead of the
 * 50km grid. Feeds Table 1, Table S2 and Figure 2c/d (via
 * R/02_table1_accounting.R, which computes consumption/formation/net-change
 * from clc_areas_change_2000_2018_countries.csv rather than from the
 * clc_areas_consumption_formation export below - see that export's note).
 *
 * Inputs (Earth Engine assets, must already exist):
 *   - projects/nina/Europe_misc/CLMS_CLCplus_RASTER_2018_010m_eu_03035_V1_1
 *   - projects/gee-zander-nina/assets/Arena/strataImg_riparian_coastal (from 01)
 *   - projects/gee-zander-nina/assets/Arena/distImg_riparian_coastal (from 02)
 *   - projects/nina/Europe_misc/CLC2000ACC_V2018_20, CLC2018ACC_V2018_20
 *   - users/zandersamuel/Global_misc/GISCO_CNT_RG_01M_2024 (country
 *     boundaries, personal GEE asset namespace - a different owner than the
 *     projects/nina and projects/gee-zander-nina paths used elsewhere)
 *
 * Outputs (Google Drive, must be downloaded manually into data/from_gee/):
 *   - clc_areas_change_2000_2018_countries.csv
 *   - clc_areas_simp_baseline_countries.csv
 *   - clc_areas_consumption_formation_2000_2018_countries.csv (exported but
 *     NOT currently read by analysis.R - the fuller accounting script
 *     derives consumption/formation directly from clc_change_country
 *     instead. Kept here because it's a useful independent cross-check of
 *     those numbers, not because the pipeline depends on it.)
 *
 * Author: Zander Venter
 */


/***
 * 1. Import datasets and define parameters (same as 03_areas_extract_grid.js) ------------------
 */
var projCrs = 'EPSG:3035'
var proj = ee.Projection(projCrs);

//// CLC+ for land mask
var clcplus = ee.Image('projects/nina/Europe_misc/CLMS_CLCplus_RASTER_2018_010m_eu_03035_V1_1');
var landMask = clcplus.neq(10).and(clcplus.neq(254))
Map.addLayer(landMask.selfMask(), {}, 'landMask', 0)

//// Strata image coastal riparian (mosaic of the per-cell assets from 01_strata_img_generate.js)
var strataImgCol = ee.ImageCollection('projects/gee-zander-nina/assets/Arena/strataImg_riparian_coastal');
var strataImg_landwater = strataImgCol.mosaic();
strataImg_landwater = strataImg_landwater.unmask(1).updateMask(landMask);
Map.addLayer(strataImg_landwater.randomVisualizer(), {}, 'strataImg_landwater', 0);

//// CLC accounting layer  ---------------------
// https://clc.gios.gov.pl/doc/clc/CLC_Legend_EN.pdf
var clc00_raw = ee.Image('projects/nina/Europe_misc/CLC2000ACC_V2018_20');
var clc18_raw = ee.Image('projects/nina/Europe_misc/CLC2018ACC_V2018_20');

// Lists CLC values and MAES values for re-mapping / cross-walk (see Table S1)
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

// Remap to MAES: 1 Urban, 2 Cropland, 3 Grassland, 4 Woodland/forest,
// 5 Heathland/shrub, 6 Sparsely vegetated, 7 Wetlands, 8 Marine inlets, 9 Rivers/lakes
var clc00 = clc00_raw.remap(clcVals, maesVals, 0);
var clc18 = clc18_raw.remap(clcVals, maesVals, 0);


/***
 * 2. Define land cover change map (same as 03_areas_extract_grid.js) --------------------------
 */
var nature00 = clc00.gt(2);
var nature18 = clc18.gt(2);

var loss_to_urban = ee.Image(0).where(nature00.and(clc18.eq(1)), 1).rename('loss_to_urban');
var gain_from_urban = ee.Image(0).where(clc00.eq(1).and(nature18), 1).rename('gain_from_urban');
var loss_to_cropland = ee.Image(0).where(nature00.and(clc18.eq(2)), 1).rename('loss_to_cropland');
var gain_from_cropland = ee.Image(0).where(clc00.eq(2).and(nature18), 1).rename('gain_from_cropland');

var combined_change = ee.Image(0)
  .where(loss_to_urban.eq(1),          10)
  .where(gain_from_urban.eq(1),        11)
  .where(loss_to_cropland.eq(1),       12)
  .where(gain_from_cropland.eq(1),     13)
  .rename('change_code')
  .toInt16();

// Add the change map to the CLC 2018 status map 9-class MAES typology
var clc_change = clc18.where(combined_change.gt(0), combined_change);


/***
 * 3. Area helper functions (same as 03_areas_extract_grid.js) ---------------------------------
 */

// Compute pixel areas by class within a geometry
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

// Cross CLC (or change codes) with land/sea strata.
// stratum = landwater * baseMultiplier + class - see 03_areas_extract_grid.js
// for the full explanation of the encoding and how it's decoded in R.
function getStratAreas(stratImage, aoi, scale, proj, baseMultiplier) {
  var strataImg_landwater_reproj = strataImg_landwater.reproject(proj.atScale(scale));
  var combined = strataImg_landwater_reproj.multiply(ee.Image(baseMultiplier)).add(stratImage);
  return getAreas(combined, aoi, scale, proj);
}


/***
 * 4. Country boundaries ------------------------------------------------------------------------
 */
var countries = ee.FeatureCollection('users/zandersamuel/Global_misc/GISCO_CNT_RG_01M_2024');
Map.addLayer(countries, {}, 'countries raw', 0)

// EEA-39 reporting countries (ISO3 codes)
var selectedEEA = [
    "ALB","AUT","BEL","BIH","BGR","CZE","CYP","DEU","DNK","ESP","EST","FIN",
    "FRA","GBR","GRC","HRV","HUN","IRL","ITA","ISL","LIE","LTU","LUX","LVA",
    "MKD","MLT","MNE","NLD","NOR","POL","PRT","ROU","SRB","SVK","SVN","SWE",
    "TUR","XKX","CHE"]
print(selectedEEA.length)

// GISCO country polygons include overseas territories (e.g. French Guiana,
// Svalbard) that fall outside the EEA-39 reporting area covered by the CLC
// Accounting Layers - clip them out with this exclusion geometry before
// aggregating, otherwise those areas would silently contribute zero/garbage
// CLC pixels to a country's totals.
var excludeArea = /* color: #98ff00 */ee.Geometry({
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
Map.addLayer(countries, {}, 'countries filtered', 0)


/***
 * 5. Aggregate areas to countries ------------------------------------------------------
 */

//// CLC gross changes and status, per country -------------------------------------------
// Same encoding as clc_areas_change_2000_2018_grid_50km.csv (baseMultiplier 14),
// but grouped by ISO3 country code instead of grid cell id.
var countryChangeOut = countries.map(function(ft){
  var stratAreas = getStratAreas(clc_change, ft.geometry(),  100, clc00.projection(), 14);
  stratAreas = stratAreas.map(function(i){
    return i.set('country', ft.get('ISO3_CODE'))
  })
  return stratAreas
}).flatten()

Export.table.toDrive({
  collection: countryChangeOut,
  fileFormat: 'CSV',
  description: 'clc_areas_change_2000_2018_countries',
})


//// Baseline (2000) simplified land cover per country ----------------------
// 1 urban, 2 cropland, 3 nature - used as the opening stock for the
// consumption/formation/net-change accounting table (Table 1, Table S2).
var baselineLCsimp = ee.Image(0)
  .where(clc00.eq(1), 1) // urban
  .where(clc00.eq(2), 2) // cropland
  .where(clc00.gt(2), 3) // nature

var countryBaseline = countries.map(function(ft){
  var areasSub = getStratAreas(baselineLCsimp,  ft.geometry(),  100, clc00.projection(), 4);
  areasSub = areasSub.map(function(i){
    return i.set('country', ft.get('ISO3_CODE'))
  })
  return areasSub
}).flatten();

Export.table.toDrive({
  collection: countryBaseline,
  fileFormat: 'CSV',
  description: 'clc_areas_simp_baseline_countries'
})


//// Consumption and formation ecosystem accounts, per country ----------------------
// Aiming for something like table 2 here: https://www.mdpi.com/2073-445X/13/9/1350
// NOTE: not currently read by analysis.R (see file header) - kept as an
// independent cross-check of the consumption/formation numbers derived
// from clc_areas_change_2000_2018_countries.csv.
//
// Consumption/formation per urban/cropland/nature are not mutually
// exclusive (e.g. a pixel can consume nature and simultaneously form
// cropland), so each transition is computed as its own binary image and
// aggregated separately via an ImageCollection rather than packed into one
// integer-coded raster.
var transitionCol = ee.ImageCollection([
  // urban formation
  ee.Image(0).where(clc00.neq(1).and(clc18.eq(1)), 1).set('label', 'urban_formation'),
  // urban consumption
  ee.Image(0).where(clc00.eq(1).and(clc18.neq(1)), 1).set('label', 'urban_consumption'),
  // cropland formation
  ee.Image(0).where(clc00.neq(2).and(clc18.eq(2)), 1).set('label', 'cropland_formation'),
  // cropland consumption
  ee.Image(0).where(clc00.eq(2).and(clc18.neq(2)), 1).set('label', 'cropland_consumption'),
  // nature formation
  ee.Image(0).where(clc00.lte(2).and(clc18.gt(2)), 1).set('label', 'nature_formation'),
  // nature consumption
  ee.Image(0).where(clc00.gt(2).and(clc18.lte(2)), 1).set('label', 'nature_consumption'),
  ])
print(transitionCol)

var countryTransitionOut = countries.map(function(ft){
  var stratAreas = transitionCol.map(function(img){
    var areasSub = getStratAreas(img,  ft.geometry(),  100, clc00.projection(), 2);
    areasSub = areasSub.map(function(i){
      return i.set('label', img.get('label'))
    })
    return areasSub
  }).flatten();

  stratAreas = stratAreas.map(function(i){
      return i.set('country', ft.get('ISO3_CODE'))
    })
  return stratAreas
}).flatten()

Export.table.toDrive({
  collection: countryTransitionOut,
  fileFormat: 'CSV',
  description: 'clc_areas_consumption_formation_2000_2018_countries'
})
