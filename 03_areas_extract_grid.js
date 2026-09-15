/**
 * 03 - Extract land-cover change areas aggregated to the 50km reporting grid
 *
 * Pipeline step 3 of 4 (Google Earth Engine, run manually in the Code
 * Editor, after 01 and 02 have finished exporting their asset collections).
 * See README.md for the full pipeline order and which downloaded files
 * this feeds into in R.
 *
 * What this does:
 *   1. Loads the strata (01) and distance (02) image collections, and the
 *      Corine Land Cover Accounting Layers (CLC AL) for 2000 and 2018,
 *      remapped to the 9-class MAES typology (Table S1).
 *   2. Derives four change layers: nature->urban, urban->nature,
 *      nature->cropland, cropland->nature (2000 to 2018).
 *   3. Exports three rasters (to Google Drive) used only for mapping in R
 *      (Figure 1a/b): the strata map, the 2018 MAES status map, and the
 *      simplified change map.
 *   4. Aggregates change + status areas to the 50km grid cells, both as
 *      gross totals and as concentric 1-10km buffer distance bands from
 *      the water's edge, and exports the resulting tables to Drive.
 *
 * Inputs (Earth Engine assets, must already exist):
 *   - projects/nina/Arena/export_grid_50km_landwater (the analysis grid -
 *     NOT the same as export_grid_EEA39 used in 01/02, which only chunks
 *     those exports)
 *   - projects/nina/Europe_misc/CLMS_CLCplus_RASTER_2018_010m_eu_03035_V1_1
 *   - projects/gee-zander-nina/assets/Arena/strataImg_riparian_coastal (from 01)
 *   - projects/gee-zander-nina/assets/Arena/distImg_riparian_coastal (from 02)
 *   - projects/nina/Europe_misc/CLC2000ACC_V2018_20, CLC2018ACC_V2018_20
 *     (CLC Accounting Layers; https://clc.gios.gov.pl/doc/clc/CLC_Legend_EN.pdf)
 *
 * Outputs (Google Drive, must be downloaded manually into data/from_gee/):
 *   - landwater_500m.tif
 *   - clc18_maes_1000m.tif
 *   - clc18_maes_change_500m.tif
 *   - clc_areas_change_2000_2018_grid_50km.csv
 *   - clc_areas_change_from_to_l3_2000_2018_grid_50km.csv
 *   - clc_areas_change_distance_2000_2018_grid_50km.csv
 *
 * Author: Zander Venter
 */


/***
 * 1. Import datasets and define parameters --------------------------------------------------------------
 */
var projCrs = 'EPSG:3035'
var proj = ee.Projection(projCrs);

var imgExportGeom =
    ee.Geometry.Polygon(
        [[[-25.17944573064396, 71.36412568115003],
          [-25.17944573064396, 34.39793899220542],
          [45.30883551935604, 34.39793899220542],
          [45.30883551935604, 71.36412568115003]]], null, false);

// Export grid
var grid = ee.FeatureCollection('projects/nina/Arena/export_grid_50km_landwater').sort('id')
Map.addLayer(grid, {}, 'grid', 0)

//// CLC+ for land mask
var clcplus = ee.Image('projects/nina/Europe_misc/CLMS_CLCplus_RASTER_2018_010m_eu_03035_V1_1');
var landMask = clcplus.neq(10).and(clcplus.neq(254))
Map.addLayer(landMask.selfMask(), {}, 'landMask', 0)


//// Strata image coastal riparian (mosaic of the per-cell assets from 01_strata_img_generate.js)
var strataImgCol = ee.ImageCollection('projects/gee-zander-nina/assets/Arena/strataImg_riparian_coastal');
var strataImg_landwater = strataImgCol.mosaic();
strataImg_landwater = strataImg_landwater.unmask(1).updateMask(landMask);
Map.addLayer(strataImg_landwater.randomVisualizer(), {}, 'strataImg_landwater', 0);

//// Distance to water (mosaic of the per-cell assets from 02_distance_img_generate.js)
var distCol = ee.ImageCollection('projects/gee-zander-nina/assets/Arena/distImg_riparian_coastal').mosaic();
var distCoast = distCol.select(0)
Map.addLayer(distCoast, {min:0, max:10000}, 'distCoast', 0)

var distRip = distCol.select(1)
Map.addLayer(distRip, {min:0, max:10000}, 'distRip', 0)

var distWater = ee.ImageCollection([distRip.rename('distWater'), distCoast.rename('distWater')]).min()
Map.addLayer(distWater, {min:0, max:10000}, 'distWater', 0)


//// CLC accounting layer  ---------------------
// https://clc.gios.gov.pl/doc/clc/CLC_Legend_EN.pdf
var clc00_raw = ee.Image('projects/nina/Europe_misc/CLC2000ACC_V2018_20');
print(clc00_raw.projection())
var clc06_raw = ee.Image('projects/nina/Europe_misc/CLC2006ACC_V2018_20');
var clc12_raw = ee.Image('projects/nina/Europe_misc/CLC2012ACC_V2018_20');
var clc18_raw = ee.Image('projects/nina/Europe_misc/CLC2018ACC_V2018_20');
Map.addLayer(clc00_raw.randomVisualizer(), {}, 'clc 2000', 0)
Map.addLayer(clc18_raw.randomVisualizer(), {}, 'clc 2018', 0)

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

// Visualization params for MAES
var maesVis = {
  min: 1,
  max: 9,
  palette: [
    '#CC0303', // 1 Urban  -> Artificial land
    '#CDB400', // 2 Cropland
    '#92AF1F', // 3 Grassland
    '#235123', // 4 Woodland and forest
    '#B76124', // 5 Heathland and shrub
    '#F7E174', // 6 Sparsely vegetated land (Bare land)
    '#c493f5', // 7 Wetlands (re-using "Water" blue for contrast)
    '#AEC3D6', // 8 Marine inlets & transitional waters (paler water)
    '#2019A4'  // 9 Rivers and lakes (same as Wetlands for water)
  ]
};

// Display
Map.addLayer(clc18, maesVis, 'clc18 maes', 0);

/***
 * 2. Define land cover change map --------------------------------------------------------------
 */
// Nature = anything other than urban (1) or cropland (2)
var nature00 = clc00.gt(2);
var nature18 = clc18.gt(2);

//// Change layers
// 1) Land take (nature -> urban)
var loss_to_urban = ee.Image(0)
  .where(nature00.and(clc18.eq(1)), 1)
  .rename('loss_to_urban');

Map.addLayer(loss_to_urban.focal_max().selfMask().updateMask(strataImg_landwater.gt(1)), {palette:['#eb56ff']}, 'urbanization rip/coast', 0)

// 2) Urban abandonment (urban -> nature)
var gain_from_urban = ee.Image(0)
  .where(clc00.eq(1).and(nature18), 1)
  .rename('gain_from_urban');
Map.addLayer(gain_from_urban.focal_max().selfMask().updateMask(strataImg_landwater.gt(1)), {palette:['#19b8f7']}, 'urb aband rip/coast', 0)

// 3) Cropland expansion (nature -> cropland)
var loss_to_cropland = ee.Image(0)
  .where(nature00.and(clc18.eq(2)), 1)
  .rename('loss_to_cropland');
Map.addLayer(loss_to_cropland.focal_max().selfMask().updateMask(strataImg_landwater.gt(1)), {palette:['#eb56ff']}, 'crop exp rip/coast', 0)

// 4) Cropland abandonment (cropland -> nature)
var gain_from_cropland = ee.Image(0)
  .where(clc00.eq(2).and(nature18), 1)
  .rename('gain_from_cropland');
Map.addLayer(gain_from_cropland.focal_max().selfMask().updateMask(strataImg_landwater.gt(1)), {palette:['#19b8f7']}, 'crop aband rip/coast', 0)

// NOTE: forest loss/gain (5, 6 below) were part of an earlier exploratory
// pass but are not used in the manuscript (which does not include
// forestry as a driver) - left here disabled for reference only.
/*
// Convenience masks for forest/non-forest
var isForest00 = clc00_raw.gte(311).and(clc00_raw.lte(313));
var isForest18 = clc18_raw.gte(311).and(clc18_raw.lte(313));

// 5) Forest loss (forest 2000 -> non-forest 2018), excluding urban/cropland outcomes already captured
var loss_to_forestry = ee.Image(0)
  .where(isForest00.and(isForest18.not()), 1)
  .where(loss_to_urban.or(loss_to_cropland), 0)
  .rename('loss_to_forestry');   // forest loss

// 6) Forest gain (non-forest 2000 -> forest 2018), excluding gains to nature via urban/cropland abandonment
var gain_from_forestry = ee.Image(0)
  .where(isForest00.not().and(isForest18), 1)
  .where(gain_from_urban.or(gain_from_cropland), 0)
  .rename('gain_from_forestry'); // forest gain
*/

var combined_change = ee.Image(0)
  .where(loss_to_urban.eq(1),          10)
  .where(gain_from_urban.eq(1),        11)
  .where(loss_to_cropland.eq(1),       12)
  .where(gain_from_cropland.eq(1),     13)
  //.where(loss_to_forestry.eq(1),       14)
  //.where(gain_from_forestry.eq(1),     15)
  .rename('change_code')
  .toInt16();

// Visualization dictionary for change_code
var changeVis = {
  min: 10,
  max: 13,
  palette: [
    '#CC0303', // 10 Loss to urban - bright red
    '#FF6666', // 11 Gain from urban - light red/pink

    '#CDB400', // 12 Loss to cropland - golden yellow
    '#FFE680', // 13 Gain from cropland - light yellow
  ]
};

// Add to map
Map.addLayer(combined_change.selfMask().focal_mode(), changeVis, 'combined_change', 0);

// Add the change map to the CLC 2018 status map 9-class MAES typology
var clc_change = clc18.where(combined_change.gt(0), combined_change);
Map.addLayer(clc_change.randomVisualizer(), {}, 'clc_change', 0);

// Add the change map to a simplified typology CLC 2018 status map
// 1 nature, 2 cropland, 3 urban, 4 gain from urban, 5 gain from cropland,
// 6 loss to urban, 7 loss to cropland
var clc_change_simple = ee.Image(0)
  .where(clc18.gt(2).and(clc18.lt(8)), 1) // natural excluding water
  .where(clc18.eq(2), 2) // cropland
  .where(clc18.eq(1), 3) // urban
  .where(combined_change.eq(11), 4) // gain from urban
  .where(combined_change.eq(13), 5) // gain from cropland
  .where(combined_change.eq(10), 6) // loss from urban
  .where(combined_change.eq(12), 7) // loss from cropland
  .rename('change_code')


/***
 * 3. Export images for mapping in R (Figure 1a/b) ------------------------------------------------------
 */
// strataImg_landwaterToExport: for the Fig 1a strata map ONLY. Reclassifies
// anything within 5km of the coast as "coastal" (value 2) purely so the
// map reads cleanly at continental scale - this is NOT the 1500m coastal
// buffer used in the actual area accounting (that comes from
// strataImg_landwater directly, untouched, via getStratAreas() below).
var strataImg_landwaterToExport = strataImg_landwater
  .where(distCoast.lte(5000), 2)
  .setDefaultProjection(proj.atScale(50))
  .reduceResolution(ee.Reducer.mode(), true, 100)
  .reproject(proj.atScale(500));
Map.addLayer(strataImg_landwaterToExport.randomVisualizer(), {}, 'strataImg_landwaterToExport', 0);

var clc18ToExport = clc18
  .reduceResolution(ee.Reducer.mode(), true, 100)
  .reproject(proj.atScale(1000));

var clc_rast_export = clc_change_simple
  .setDefaultProjection(proj.atScale(100))
  // getting max to highlight change
  .reduceResolution(ee.Reducer.max(), true, 100)
  .reproject(proj.atScale(500));

Export.image.toDrive({
  image: strataImg_landwaterToExport,
  description: 'landwater_500m',
  scale: 500,
  region: imgExportGeom,
  crs: projCrs,
  maxPixels: 1e10
})
Export.image.toDrive({
  image: clc18ToExport,
  description: 'clc18_maes_1000m',
  scale: 1000,
  region: imgExportGeom,
  crs: projCrs
})
Export.image.toDrive({
  image: clc_rast_export,
  description: 'clc18_maes_change_500m',
  scale: 500,
  region: imgExportGeom,
  crs: projCrs,
  maxPixels: 1e10
})


/***
 * 4. Aggregate areas to grid  ------------------------------------------------------
 */

// Compute pixel areas by class within a geometry
function getAreas(stratImage, aoi, scale, proj) {
  // stratImage: single-band image of integer class codes
  // aoi: ee.Geometry to summarise
  // scale: target scale (e.g. 100 m)
  // proj: projection of the input classes (e.g. clc00.projection())

  // 1. Add pixelArea as band 0, class codes as band 1
  var strataAreas = ee.Image.pixelArea()
    .addBands(stratImage)
    .reproject(proj.atScale(scale)) // ensure consistent scale
    .reduceRegion({
      reducer: ee.Reducer.sum().group(1), // group by band 1 = class code
      geometry: aoi,
      scale: scale,
      maxPixels: 1e14,
      bestEffort: true
    });

  // 2. Extract grouped results as a FeatureCollection
  var groups = ee.List(strataAreas.get('groups'));
  var strataInfo = groups.map(function(group) {
    var dict = ee.Dictionary(group)
      .rename(['sum', 'group'], ['area', 'stratum']); // rename keys
    return ee.Feature(null, dict); // area in m^2, stratum is class code
  });

  return ee.FeatureCollection(strataInfo);
}

// Cross CLC (or change codes) with land/sea strata.
// Encodes stratum = landwater * baseMultiplier + class, where landwater is
// 1 (inland) / 2 (coastal) / 3 (riparian) from strataImg_landwater, and
// class is whatever integer code stratImage carries. baseMultiplier must
// be strictly greater than the max class code in stratImage so the two
// can be decoded again in R via `%/%` and `%%` (see analysis.R's 00_setup.R).
function getStratAreas(stratImage, aoi, scale, proj, baseMultiplier) {
  // Reproject land-water strata to match target scale
  var strataImg_landwater_reproj = strataImg_landwater.reproject(proj.atScale(scale));

  var combined = strataImg_landwater_reproj
    .multiply(ee.Image(baseMultiplier))
    .add(stratImage);

  return getAreas(combined, aoi, scale, proj);
}


//// CLC gross changes and status -------------------------------------------
// baseMultiplier = 14: clc_change classes run 1-13 (9 MAES classes + 4 change codes)
var gridAreas = grid.map(function(ft){
  var stratAreas = getStratAreas(clc_change, ft.geometry(),  100, clc00.projection(), 14);
  stratAreas = stratAreas.map(function(i){ return i.set('id', ft.get('id'))})
  return stratAreas
}).flatten()

Export.table.toDrive({
  collection: gridAreas,
  fileFormat: 'CSV',
  description: 'clc_areas_change_2000_2018_grid_50km'
})

//// CLC changes detailed typology - CLC 2018 level 3  --------------------------------------
// Answers: what did changed areas change to/from, at full CLC level-3 detail
// (used for Figure 5 - post-loss/post-recovery and pre-loss/pre-recovery land cover shares)

var clc18_raw_masked_test = clc18_raw.updateMask(combined_change.eq(1));
Map.addLayer(clc18_raw_masked_test.randomVisualizer(), {}, 'clc00_raw_masked_test',0);

// Define the list of change values you will use to mask CLC18 with
var indexList = ee.List([10,11,12,13]);

var changeOutL3 = indexList.map(function(index){
  index= ee.Number.parse(index)
  var clc18_raw_masked = clc18_raw.selfMask().updateMask(combined_change.eq(ee.Image(index)));
  var clc00_raw_masked = clc00_raw.selfMask().updateMask(combined_change.eq(ee.Image(index)));

  // Post-change (2018) land cover for pixels that underwent this change
  var gridAreas_changeTo = grid.map(function(ft){
    var stratAreas = getStratAreas(clc18_raw_masked, ft.geometry(),  100, clc00.projection(), 524);
    stratAreas = stratAreas.map(function(i){
      return i.set('id', ft.get('id'), 'change_code', index, 'change_type', 'changed_to_2018')
    })
    return stratAreas
  }).flatten()

  // Pre-change (2000) land cover for the same pixels
  var gridAreas_changeFrom = grid.map(function(ft){
    var stratAreas = getStratAreas(clc00_raw_masked, ft.geometry(),  100, clc00.projection(), 524);
    stratAreas = stratAreas.map(function(i){
      return i.set('id', ft.get('id'), 'change_code', index, 'change_type', 'changed_from_2000')
    })
    return stratAreas
  }).flatten()

  return ee.FeatureCollection(gridAreas_changeTo).merge(ee.FeatureCollection(gridAreas_changeFrom))
});
changeOutL3 = ee.FeatureCollection(changeOutL3).flatten();

Export.table.toDrive({
  collection: changeOutL3,
  fileFormat: 'CSV',
  description: 'clc_areas_change_from_to_l3_2000_2018_grid_50km'
})

//// CLC changes by distance to coastal, riparian and coastal in bands----------------------------------------
// Cumulative area within <= 1,2,...,10 km of the water's edge (converted
// to per-band area via lag() differencing in R - see 00_setup.R).
var distList = ee.List([1000,2000,3000,4000,5000,6000,7000,8000,9000,10000]);

function getDistOutput(distImg, label){
  var changeOutDist = distList.map(function(dist){
    dist= ee.Number.parse(dist);
    var clc_change_masked = clc_change_simple.updateMask(distImg.lte(ee.Image(dist)));
    var gridAreas = grid.map(function(ft){
      var stratAreas = getAreas(clc_change_masked, ft.geometry(),  100, clc00.projection());
      stratAreas = stratAreas.map(function(i){ return i.set('id', ft.get('id'), 'distance', dist, 'type', label)})
      return stratAreas
    }).flatten()
    return ee.FeatureCollection(gridAreas)
  });
  changeOutDist = ee.FeatureCollection(changeOutDist).flatten();
  return changeOutDist
}

var changeDistCoast_bands = getDistOutput(distCoast, 'coastal');
var changeDistRiparian_bands = getDistOutput(distRip, 'riparian');
var changeDistWater_bands = getDistOutput(distWater, 'all');

var changeDistAll_bands = changeDistCoast_bands.merge(changeDistRiparian_bands).merge(changeDistWater_bands)

Export.table.toDrive({
  collection: changeDistAll_bands,
  fileFormat: 'CSV',
  description: 'clc_areas_change_distance_2000_2018_grid_50km'
})


/// Map interaction - useful for inspecting changes on the map using Google Earth or Esri wayback
Map.setOptions('HYBRID')
Map.onClick(handleMapClick)
function handleMapClick(coords){

  coords = [coords.lon, coords.lat]
  var googleURL = 'https://earth.google.com/web/@'+String(coords[1]) +','+String(coords[0]) +',156.58634283a,1165.64764158d,35y,0h,0t,0r'
  var livingatlasURL = 'https://livingatlas.arcgis.com/wayback/#active=25982&mapCenter='+String(coords[0]) +'%2C' +String(coords[1]) +'%2C17'
  var planetURL = 'https://www.planet.com/basemaps/#/mode/compare/mosaic/global_quarterly_2018q3_mosaic/comparison/global_quarterly_2023q3_mosaic/center/'+String(coords[0]) +','+String(coords[1]) +'/zoom/17'


  var panel = ui.Panel({style:{position:'top-left', width:'350px'}})
  panel.add(ui.Label('googleURL',null,googleURL))
  .add(ui.Label('livingatlasURL',null,livingatlasURL))
  .add(ui.Label('planetURL',null,planetURL))
  Map.widgets().reset([panel]);
}
