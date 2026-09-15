/**
 * 05 - Extract land-cover change areas per CLC 6-year epoch (2000-06,
 * 2006-12, 2012-18), aggregated to the 50km reporting grid
 *
 * Revision addition (response to Reviewer 2's request for a less discrete,
 * more temporally resolved change signal than a single 2000-2018 window).
 * Same change-layer derivation and stratum encoding as
 * 03_areas_extract_grid.js's "clc_areas_change_2000_2018_grid_50km" export
 * - repeated for each of the three CLC Accounting Layer reference years
 * available (2000, 2006, 2012, 2018) instead of collapsing straight from
 * 2000 to 2018. Biome is NOT joined here - R already has a grid-id ->
 * biome lookup (ecoregion_lookup in R/00_setup.R), so this just needs to
 * export per-epoch, per-grid-cell, per-landwater-zone change areas and let
 * R do the biome aggregation, exactly as it already does for Figure 4.
 *
 * Run after 01 and 02 (needs their asset collections). Independent of 03/04
 * - does not need their Drive exports, just the same upstream EE assets.
 *
 * Inputs (Earth Engine assets, must already exist):
 *   - projects/nina/Arena/export_grid_50km_landwater
 *   - projects/nina/Europe_misc/CLMS_CLCplus_RASTER_2018_010m_eu_03035_V1_1
 *   - projects/gee-zander-nina/assets/Arena/strataImg_riparian_coastal (from 01)
 *   - projects/nina/Europe_misc/CLC2000ACC_V2018_20, CLC2006ACC_V2018_20,
 *     CLC2012ACC_V2018_20, CLC2018ACC_V2018_20 (CLC Accounting Layers -
 *     2006/2012 were already loaded but unused in 03_areas_extract_grid.js)
 *
 * Output (Google Drive, download into data/from_gee/):
 *   - clc_areas_change_epochs_grid_50km.csv (same stratum encoding as
 *     clc_areas_change_2000_2018_grid_50km.csv - landwater*14 + clc_class -
 *     plus an `epoch` property: "2000_2006", "2006_2012", or "2012_2018")
 *
 * Author: Zander Venter
 */


/***
 * 1. Import datasets and define parameters (same as 03_areas_extract_grid.js) ------------------
 */
var projCrs = 'EPSG:3035'
var proj = ee.Projection(projCrs);

var grid = ee.FeatureCollection('projects/nina/Arena/export_grid_50km_landwater').sort('id')
Map.addLayer(grid, {}, 'grid', 0)

var clcplus = ee.Image('projects/nina/Europe_misc/CLMS_CLCplus_RASTER_2018_010m_eu_03035_V1_1');
var landMask = clcplus.neq(10).and(clcplus.neq(254))

var strataImgCol = ee.ImageCollection('projects/gee-zander-nina/assets/Arena/strataImg_riparian_coastal');
var strataImg_landwater = strataImgCol.mosaic();
strataImg_landwater = strataImg_landwater.unmask(1).updateMask(landMask);

//// CLC accounting layers for all four reference years -----------------------
var clc00_raw = ee.Image('projects/nina/Europe_misc/CLC2000ACC_V2018_20');
var clc06_raw = ee.Image('projects/nina/Europe_misc/CLC2006ACC_V2018_20');
var clc12_raw = ee.Image('projects/nina/Europe_misc/CLC2012ACC_V2018_20');
var clc18_raw = ee.Image('projects/nina/Europe_misc/CLC2018ACC_V2018_20');

// Same CLC -> MAES crosswalk as 03_areas_extract_grid.js (Table S1)
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
var clc06 = clc06_raw.remap(clcVals, maesVals, 0);
var clc12 = clc12_raw.remap(clcVals, maesVals, 0);
var clc18 = clc18_raw.remap(clcVals, maesVals, 0);


/***
 * 2. Change-layer + status/change map builder (reusable per epoch) --------------
 * Same logic as 03_areas_extract_grid.js's section 2, factored into a
 * function so it can be applied to each of the three 6-year epochs.
 */
function buildChangeSimple(clcStart, clcEnd) {
  var natureStart = clcStart.gt(2);
  var natureEnd = clcEnd.gt(2);

  var loss_to_urban = ee.Image(0).where(natureStart.and(clcEnd.eq(1)), 1);
  var gain_from_urban = ee.Image(0).where(clcStart.eq(1).and(natureEnd), 1);
  var loss_to_cropland = ee.Image(0).where(natureStart.and(clcEnd.eq(2)), 1);
  var gain_from_cropland = ee.Image(0).where(clcStart.eq(2).and(natureEnd), 1);

  var combined_change = ee.Image(0)
    .where(loss_to_urban.eq(1),      10)
    .where(gain_from_urban.eq(1),    11)
    .where(loss_to_cropland.eq(1),   12)
    .where(gain_from_cropland.eq(1), 13)
    .rename('change_code')
    .toInt16();

  // Simplified typology: 1 nature, 2 cropland, 3 urban, 4 gain from urban,
  // 5 gain from cropland, 6 loss to urban, 7 loss to cropland (end-of-epoch
  // status, matching clc_change_simple in 03_areas_extract_grid.js)
  var clc_change_simple = ee.Image(0)
    .where(clcEnd.gt(2).and(clcEnd.lt(8)), 1)
    .where(clcEnd.eq(2), 2)
    .where(clcEnd.eq(1), 3)
    .where(combined_change.eq(11), 4)
    .where(combined_change.eq(13), 5)
    .where(combined_change.eq(10), 6)
    .where(combined_change.eq(12), 7)
    .rename('change_code');

  // Same "status + change" composite used for clc_areas_change_*.csv: end-
  // of-epoch MAES status, overwritten with the change code where a
  // transition occurred (so stable-nature/cropland/urban baseline area is
  // still exported alongside the four change classes).
  var clc_change = clcEnd.where(combined_change.gt(0), combined_change);

  return clc_change;
}


/***
 * 3. Area helper functions (identical to 03_areas_extract_grid.js) --------------
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

// baseMultiplier=14, same encoding as clc_areas_change_2000_2018_grid_50km.csv
function getStratAreas(stratImage, aoi, scale, proj, baseMultiplier) {
  var strataImg_landwater_reproj = strataImg_landwater.reproject(proj.atScale(scale));
  var combined = strataImg_landwater_reproj.multiply(ee.Image(baseMultiplier)).add(stratImage);
  return getAreas(combined, aoi, scale, proj);
}


/***
 * 4. Build each epoch's change layer and aggregate to grid ----------------------
 */
var epochs = [
  {label: '2000_2006', clcStart: clc00, clcEnd: clc06},
  {label: '2006_2012', clcStart: clc06, clcEnd: clc12},
  {label: '2012_2018', clcStart: clc12, clcEnd: clc18}
];

var epochAreas = epochs.map(function(epoch) {
  var clc_change_epoch = buildChangeSimple(epoch.clcStart, epoch.clcEnd);

  var gridAreas = grid.map(function(ft) {
    var stratAreas = getStratAreas(clc_change_epoch, ft.geometry(), 100, clc00.projection(), 14);
    stratAreas = stratAreas.map(function(i) {
      return i.set('id', ft.get('id'), 'epoch', epoch.label);
    });
    return stratAreas;
  }).flatten();

  return gridAreas;
});
epochAreas = ee.FeatureCollection(epochAreas).flatten();

Export.table.toDrive({
  collection: epochAreas,
  fileFormat: 'CSV',
  description: 'clc_areas_change_epochs_grid_50km'
})
