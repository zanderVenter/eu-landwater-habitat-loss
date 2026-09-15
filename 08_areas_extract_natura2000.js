/**
 * 08 - Extract land-cover change and status areas inside vs. outside
 * Natura2000 protected sites, crossed with the coastal/riparian/inland
 * strata
 *
 * Revision addition (response to Reviewer 2's request for a stronger link
 * to EU conservation policy). Reuses the same change-layer derivation as
 * 03/04/06, crossed with a new protected/unprotected binary stratum built
 * from the Natura2000 network as of end-2017 (chosen over a current
 * download so protection status is evaluated close to the study's 2018
 * end year, not inflated by post-2018 designations - see the revision
 * plan for the reasoning). Feeds R/11_natura2000_comparison.R.
 *
 * Inputs (Earth Engine assets, must already exist):
 *   - projects/nina/Europe_misc/CLMS_CLCplus_RASTER_2018_010m_eu_03035_V1_1
 *   - projects/gee-zander-nina/assets/Arena/strataImg_riparian_coastal (from 01)
 *   - projects/nina/Europe_misc/CLC2000ACC_V2018_20, CLC2018ACC_V2018_20
 *   - <Natura2000 asset> - upload data/natura2000_end2017_simplified.shp
 *     (27,741 sites, end-2017, all designation types) via the Code Editor
 *     Assets tab, then paste the resulting asset ID in below
 *     (NATURA2000_ASSET_ID).
 *
 * Output (Google Drive, download into data/from_gee/):
 *   - clc_areas_change_2000_2018_natura2000.csv - stratum encoding is
 *     landwaterProtected*14 + clc_class, where landwaterProtected is 1-6:
 *     1 inland-unprotected, 2 inland-protected, 3 coastal-unprotected,
 *     4 coastal-protected, 5 riparian-unprotected, 6 riparian-protected
 *     (see R/00_setup.R-style decoding in R/11_natura2000_comparison.R).
 *
 * Author: Zander Venter
 */

// TODO(zander): set this to the asset ID after uploading
// data/natura2000_end2017_simplified.shp via the Code Editor Assets tab.
var NATURA2000_ASSET_ID = 'projects/nina/Europe_misc/natura2000_end2017_simplified';


/***
 * 1. Import datasets and define parameters (same as 03/04/06) ------------------
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
 * 2. Define land cover change map (same as 03/04/06) --------------------------
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
 * 3. Natura2000 protected/unprotected stratum, crossed with landwater ---------
 */
var natura2000 = ee.FeatureCollection(NATURA2000_ASSET_ID);
Map.addLayer(natura2000, {}, 'natura2000', 0)
print('natura2000 site count', natura2000.size())

var protectedImg = ee.Image(0).paint(natura2000, 1); // 1 inside a Natura2000 site

// Combine into a single 1-6 stratum so it can reuse the same
// stratum-encoding trick as everywhere else (landwaterProtected*14 + class):
// 1 inland-unprotected, 2 inland-protected, 3 coastal-unprotected,
// 4 coastal-protected, 5 riparian-unprotected, 6 riparian-protected
var landwaterProtected = ee.Image(0)
  .where(strataImg_landwater.eq(1).and(protectedImg.neq(1)), 1)
  .where(strataImg_landwater.eq(1).and(protectedImg.eq(1)),  2)
  .where(strataImg_landwater.eq(2).and(protectedImg.neq(1)), 3)
  .where(strataImg_landwater.eq(2).and(protectedImg.eq(1)),  4)
  .where(strataImg_landwater.eq(3).and(protectedImg.neq(1)), 5)
  .where(strataImg_landwater.eq(3).and(protectedImg.eq(1)),  6);
Map.addLayer(landwaterProtected.randomVisualizer(), {}, 'landwaterProtected', 0);


/***
 * 4. Area helper functions (same pattern as 03/04/06, but keyed off
 * landwaterProtected instead of strataImg_landwater) -------------------------
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
  var landwaterProtected_reproj = landwaterProtected.reproject(proj.atScale(scale));
  var combined = landwaterProtected_reproj.multiply(ee.Image(baseMultiplier)).add(stratImage);
  return getAreas(combined, aoi, scale, proj);
}


/***
 * 5. Aggregate areas continent-wide (EEA-39 export grid used only as a
 * convenient tiling mechanism for reduceRegion, same reason 01/02 tile by
 * grid cell - not for per-cell output here) ------------------------------
 */
var grid = ee.FeatureCollection('projects/nina/Arena/export_grid_50km_landwater').sort('id')

var gridAreas = grid.map(function(ft){
  var stratAreas = getStratAreas(clc_change, ft.geometry(),  100, clc00.projection(), 14);
  stratAreas = stratAreas.map(function(i){ return i.set('id', ft.get('id'))})
  return stratAreas
}).flatten()

Export.table.toDrive({
  collection: gridAreas,
  fileFormat: 'CSV',
  description: 'clc_areas_change_2000_2018_natura2000'
})
