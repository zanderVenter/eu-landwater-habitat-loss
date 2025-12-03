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

//// CLC+ for land mask ------------------------------------------
var clcplus = ee.Image('projects/nina/Europe_misc/CLMS_CLCplus_RASTER_2018_010m_eu_03035_V1_1');
var landMask = clcplus.neq(10).and(clcplus.neq(254))
Map.addLayer(landMask.selfMask(), {}, 'landMask', 0)

//// Distance to water ----------------------------------------------------
var distCol = ee.ImageCollection('projects/gee-zander-nina/assets/Arena/distImg_riparian_coastal').mosaic();
var distCoast = distCol.select(0)
Map.addLayer(distCoast, {min:0, max:10000}, 'distCoast', 0)

var distRip = distCol.select(1)
Map.addLayer(distRip, {min:0, max:10000}, 'distRip', 0)

var distWater = ee.ImageCollection([distRip.rename('distWater'), distCoast.rename('distWater')]).min()
Map.addLayer(distWater, {min:0, max:10000}, 'distWater', 0)

//// Strata image Areana ----------------------------------------------------
var strataImgCol = ee.ImageCollection('projects/gee-zander-nina/assets/Arena/strataImg_riparian_coastal');
var proj = strataImgCol.first().projection();
print(proj.nominalScale())
var strataImg_landwater = strataImgCol.mosaic();
Map.addLayer(strataImg_landwater.randomVisualizer(), {}, 'strataImg_landwater', 0);

// Export image for visualization in R
var strataImg_landwaterToExport = strataImg_landwater
  .where(distCoast.lte(5000), 2)
  .setDefaultProjection(proj.atScale(50))
  .reduceResolution(ee.Reducer.mode(), true, 100)
  .reproject(proj.atScale(500));
Map.addLayer(strataImg_landwaterToExport.randomVisualizer(), {}, 'strataImg_landwaterToExport', 0);
  
Export.image.toDrive({
  image: strataImg_landwaterToExport,
  description: 'landwater_500m',
  scale: 500,
  region: imgExportGeom,
  crs: projCrs,
  maxPixels: 1e10 
})


//// CLC accounting layer  ---------------------
// https://clc.gios.gov.pl/doc/clc/CLC_Legend_EN.pdf
var clc00_raw = ee.Image('projects/nina/Europe_misc/CLC2000ACC_V2018_20');
print(clc00_raw.projection())
var clc06_raw = ee.Image('projects/nina/Europe_misc/CLC2006ACC_V2018_20');
var clc12_raw = ee.Image('projects/nina/Europe_misc/CLC2012ACC_V2018_20');
var clc18_raw = ee.Image('projects/nina/Europe_misc/CLC2018ACC_V2018_20');
Map.addLayer(clc00_raw.randomVisualizer(), {}, 'clc 2000', 0)
Map.addLayer(clc18_raw.randomVisualizer(), {}, 'clc 2018', 0)

// Lists from your mapping table
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

// Remap
var clc00 = clc00_raw.remap(clcVals, maesVals, 0);
var clc18 = clc18_raw.remap(clcVals, maesVals, 0);

// Export image for visualization in R
var clc18ToExport = clc18
  .reduceResolution(ee.Reducer.mode(), true, 100)
  .reproject(proj.atScale(1000));
  
Export.image.toDrive({
  image: clc18ToExport,
  description: 'clc18_maes_1000m',
  scale: 1000,
  region: imgExportGeom,
  crs: projCrs
})


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

var nature00 = clc00.gt(2);
var nature18 = clc18.gt(2);

//// Change layers 
// 1) Land take (nature → urban)
var loss_to_urban = ee.Image(0)
  .where(nature00.and(clc18.eq(1)), 1)
  .rename('loss_to_urban');

Map.addLayer(loss_to_urban.focal_max().selfMask().updateMask(strataImg_landwater.gt(1)), {palette:['#eb56ff']}, 'urbanization rip/coast', 0)

// 2) Urban abandonment (urban → nature)
var gain_from_urban = ee.Image(0)
  .where(clc00.eq(1).and(nature18), 1)
  .rename('gain_from_urban');
Map.addLayer(gain_from_urban.focal_max().selfMask().updateMask(strataImg_landwater.gt(1)), {palette:['#19b8f7']}, 'urb aband rip/coast', 0)

// 3) Cropland expansion (nature → cropland)
var loss_to_cropland = ee.Image(0)
  .where(nature00.and(clc18.eq(2)), 1)
  .rename('loss_to_cropland');
Map.addLayer(loss_to_cropland.focal_max().selfMask().updateMask(strataImg_landwater.gt(1)), {palette:['#eb56ff']}, 'crop exp rip/coast', 0)

// 4) Cropland abandonment (cropland → nature)
var gain_from_cropland = ee.Image(0)
  .where(clc00.eq(2).and(nature18), 1)
  .rename('gain_from_cropland');
Map.addLayer(gain_from_cropland.focal_max().selfMask().updateMask(strataImg_landwater.gt(1)), {palette:['#19b8f7']}, 'crop aband rip/coast', 0)

// Convenience masks for forest/non-forest
var isForest00 = clc00_raw.gte(311).and(clc00_raw.lte(313));
var isForest18 = clc18_raw.gte(311).and(clc18_raw.lte(313));

// 5) Forest loss (forest 2000 → non-forest 2018), excluding urban/cropland outcomes already captured
var loss_to_forestry = ee.Image(0)
  .where(isForest00.and(isForest18.not()), 1)
  .where(loss_to_urban.or(loss_to_cropland), 0)
  .rename('loss_to_forestry');   // forest loss

// 6) Forest gain (non-forest 2000 → forest 2018), excluding gains to nature via urban/cropland abandonment
var gain_from_forestry = ee.Image(0)
  .where(isForest00.not().and(isForest18), 1)
  .where(gain_from_urban.or(gain_from_cropland), 0)
  .rename('gain_from_forestry'); // forest gain

var combined_change = ee.Image(0)
  .where(loss_to_urban.eq(1),          10)
  .where(gain_from_urban.eq(1),        11)
  .where(loss_to_cropland.eq(1),       12)
  .where(gain_from_cropland.eq(1),     13)
  .where(loss_to_forestry.eq(1),       14)
  .where(gain_from_forestry.eq(1),     15)
  .rename('change_code')
  .toInt16();

// Visualization dictionary for change_code
var changeVis = {
  min: 10,
  max: 15,
  palette: [
    '#CC0303', // 10 Loss to urban - bright red
    '#FF6666', // 11 Gain from urban - light red/pink

    '#CDB400', // 12 Loss to cropland - golden yellow
    '#FFE680', // 13 Gain from cropland - light yellow

    '#7030A0', // 14 Forest loss - purple
    '#92AF1F'  // 15 Forest gain - green
  ]
};

// Add to map
Map.addLayer(combined_change.selfMask().focal_mode(), changeVis, 'combined_change', 0);


var clc_change = clc18.where(combined_change.gt(0), combined_change);
Map.addLayer(clc_change.randomVisualizer(), {}, 'clc_change', 0);

var clc_change_simple = ee.Image(0)
  .where(clc18.gt(2).and(clc18.lt(8)), 1) // natural excluding water
  .where(clc18.eq(2), 2) // cropland
  .where(clc18.eq(1), 3) // urban
  .where(combined_change.eq(11), 4) // gain from urban
  .where(combined_change.eq(13), 5) // gain from cropland
  .where(combined_change.eq(10), 6) // loss from urban
  .where(combined_change.eq(12), 7) // loss from cropland
  .rename('change_code')
  
// Export image for visualization in R
var clc_rast_export = clc_change_simple
  .setDefaultProjection(proj.atScale(100))
  // getting max to highlight change
  .reduceResolution(ee.Reducer.max(), true, 100)
  .reproject(proj.atScale(500));

Export.image.toDrive({
  image: clc_rast_export,
  description: 'clc18_maes_change_500m',
  scale: 500,
  region: imgExportGeom,
  crs: projCrs,
  maxPixels: 1e10 
})



/***
 * 1. Aggregate to grid  ------------------------------------------------------
 */
 // Function to get strata area
function getAreas(stratImage, aoi,  scale, proj){
  // Calculate strata areas and group them
  var strataAreas = ee.Image.pixelArea().addBands(stratImage)
    .reproject(proj.atScale(scale))
    .reduceRegion({
      reducer: ee.Reducer.sum().group(1),
      geometry: aoi,
      scale: scale,
      maxPixels: 1e14,
      bestEffort: true
  });
  
  // Process groups to extract information server-side
  var groups = ee.List(strataAreas.get('groups'));
  
  var strataInfo = groups.map(function(group) {
    var dict = ee.Dictionary(group);
    dict = dict.rename(['sum', 'group'], ['area', 'stratum'])
    //var area = ee.Number(dict.get('sum'));
    return ee.Feature(null, dict);
  });
  strataInfo = ee.FeatureCollection(strataInfo)
  
  return strataInfo
}

// Function to get a areas for an stratImgage (ie. could be CLC) crossed with landwater strata
function getStratAreas(stratImage, aoi,  scale, proj, baseMultiplier){
  
  // Multiply with land water strata
  var strataImg_landwater_reproj = strataImg_landwater.reproject(proj.atScale(scale))
  stratImage = strataImg_landwater_reproj.multiply(ee.Image(baseMultiplier)).add(stratImage);
  
  //Map.addLayer(strataImg_landwater_reproj.randomVisualizer())
  //Map.addLayer(stratImage.randomVisualizer())
  
  return getAreas(stratImage, aoi,  scale, proj)
  
  
}
//getStratAreas(clc00, ee.Geometry.Rectangle(Map.getBounds()),  100, clc00.projection(), 524);


//// CLC gross changes -------------------------------------------
var gridAreas = grid.map(function(ft){
  
  var stratAreas = getStratAreas(clc_change, ft.geometry(),  100, clc00.projection(), 16);
  stratAreas = stratAreas.map(function(i){ return i.set('id', ft.get('id'))})
  
  return stratAreas
}).flatten()

Export.table.toDrive({
  collection: gridAreas,
  fileFormat: 'CSV',
  description: 'clc_areas_change_2000_2018_grid_50km'
})

//// CLC changes detailed typology - CLC 2018 level 3  --------------------------------------
// what did areas change to

var clc18_raw_masked_test = clc18_raw.updateMask(combined_change.eq(1));
Map.addLayer(clc18_raw_masked_test.randomVisualizer(), {}, 'clc00_raw_masked_test',0);

var indexList = ee.List([10,11,12,13,14,15]);

var changeOutL3 = indexList.map(function(index){
  index= ee.Number.parse(index)
  var clc18_raw_masked = clc18_raw.selfMask().updateMask(combined_change.eq(ee.Image(index)));
  
  var gridAreas = grid.map(function(ft){
    var stratAreas = getStratAreas(clc18_raw_masked, ft.geometry(),  100, clc00.projection(), 524);
    stratAreas = stratAreas.map(function(i){ return i.set('id', ft.get('id'), 'change_code', index)})
    
    return stratAreas
  }).flatten()
  
  return ee.FeatureCollection(gridAreas)
});
changeOutL3 = ee.FeatureCollection(changeOutL3).flatten();

Export.table.toDrive({
  collection: changeOutL3,
  fileFormat: 'CSV',
  description: 'clc_areas_change_only_l3_2000_2018_grid_50km'
})



//// CLC changes by distance to coastal, riparian and coastal in bands----------------------------------------
var distList = ee.List([1000,2000,3000,4000,5000,6000,7000,8000,9000,10000]);
//var distList = ee.List([250, 500]);

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
