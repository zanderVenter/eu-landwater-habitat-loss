/**
 * 06 - Extract land-cover change and status areas aggregated to major
 * European river catchments
 *
 * Revision addition (response to Reviewer 2's request for a major-watershed
 * breakdown). Same change-layer derivation and stratum encoding as
 * 04_areas_extract_countries.js's "clc_areas_change_2000_2018_countries"
 * export, grouped by basin instead of country. Feeds a new supplementary
 * figure reproducing Figure 4's biome-stratified panel layout with basins
 * in place of biomes (R/09_figureS_basins.R).
 *
 * Inputs (Earth Engine assets, must already exist):
 *   - projects/nina/Europe_misc/CLMS_CLCplus_RASTER_2018_010m_eu_03035_V1_1
 *   - projects/gee-zander-nina/assets/Arena/strataImg_riparian_coastal (from 01)
 *   - projects/nina/Europe_misc/CLC2000ACC_V2018_20, CLC2018ACC_V2018_20
 *   - <basins asset> - upload data/basins_eu_hydro_v013.shp (33 EEA
 *     catchments, dissolved from the EU-Hydro RiverBasins layer, field
 *     `basin_name`) via the Code Editor Assets tab, then paste the
 *     resulting asset ID in below (BASINS_ASSET_ID).
 *
 * Output (Google Drive, download into data/from_gee/):
 *   - clc_areas_change_2000_2018_basins.csv (same stratum encoding as
 *     clc_areas_change_2000_2018_countries.csv - landwater*14 + clc_class -
 *     with a `basin_name` property instead of `country`)
 *
 * Author: Zander Venter
 */

// TODO(zander): set this to the asset ID after uploading
// data/basins_eu_hydro_v013.shp via the Code Editor Assets tab.
var BASINS_ASSET_ID = 'projects/nina/Europe_misc/basins_eu_hydro_v013';


/***
 * 1. Import datasets and define parameters (same as 03/04) ------------------
 */
var projCrs = 'EPSG:3035'
var proj = ee.Projection(projCrs);

var clcplus = ee.Image('projects/nina/Europe_misc/CLMS_CLCplus_RASTER_2018_010m_eu_03035_V1_1');
var landMask = clcplus.neq(10).and(clcplus.neq(254))

var strataImgCol = ee.ImageCollection('projects/gee-zander-nina/assets/Arena/strataImg_riparian_coastal');
var strataImg_landwater = strataImgCol.mosaic();
strataImg_landwater = strataImg_landwater.unmask(1).updateMask(landMask);

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


/***
 * 2. Define land cover change map (same as 03/04) --------------------------
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

var clc_change = clc18.where(combined_change.gt(0), combined_change);


/***
 * 3. Area helper functions (same as 03/04) ---------------------------------
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
 * 4. Major catchment boundaries ---------------------------------------------
 */
var basins = ee.FeatureCollection(BASINS_ASSET_ID);
Map.addLayer(basins, {}, 'basins', 0)
print('basin count', basins.size())


/***
 * 5. Aggregate areas to basins ------------------------------------------------
 */
// Same encoding as clc_areas_change_2000_2018_countries.csv (baseMultiplier 14),
// grouped by basin_name instead of ISO3 country code.
var basinChangeOut = basins.map(function(ft){
  var stratAreas = getStratAreas(clc_change, ft.geometry(),  100, clc00.projection(), 14);
  stratAreas = stratAreas.map(function(i){
    return i.set('basin_name', ft.get('basin_name'))
  })
  return stratAreas
}).flatten()

Export.table.toDrive({
  collection: basinChangeOut,
  fileFormat: 'CSV',
  description: 'clc_areas_change_2000_2018_basins',
})
