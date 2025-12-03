var geometry = /* color: #d63000 */ee.Geometry.Point([8.288013040940147, 46.78275013303955]);
Map.setOptions('HYBRID');

var projCrs = 'EPSG:3035'
var proj = ee.Projection(projCrs).atScale(10);

/***
 * 1. Import the land cover and coastal/riparian maps to define strata -----------------------------------
 */

//  CLC+
var lcVizDict = {
  "names": [
    "Sealed", //1
    "Woody", //2
    "Low-growing woody", //3
    "Permanent herbaceous",//4
    "Periodically herbaceous", //5
    "Sparesely vegetated", //6
    "Water", //7
  ],
  "colors": [
    "#CC0303", 
    "#235123",
    "#B76124", 
    "#92AF1F",
    "#CDB400", 
    "#F7E174",
    "#2019A4",
  ]};

var clcplus = ee.Image('projects/nina/Europe_misc/CLMS_CLCplus_RASTER_2018_010m_eu_03035_V1_1');
clcplus = clcplus.remap(
  [1,2,3,4,5,6,7,8,9,10,11],
  [1,2,2,2,3,4,5,6,6, 7, 7]);
  
Map.addLayer(clcplus, {min:1, max:7, palette: lcVizDict.colors}, 'clcplus', 0);

var clcWater = clcplus.eq(7);
Map.addLayer(clcWater, {min:0, max:1}, 'clcWater', 0)

//// Riparian -------------------------------------------------------
// https://land.copernicus.eu/en/technical-library/product-user-manual-riparian-zones-land-cover-land-use-2012-and-2018-and-land-cover-land-use-change-2012-2018/@@download/file
var riparian = ee.FeatureCollection('projects/nina/Europe_misc/CLMS_riparian_zone/rpz_DU001A_2018')
  .merge(ee.FeatureCollection('projects/nina/Europe_misc/CLMS_riparian_zone/rpz_DU002A_2018'))
  .merge(ee.FeatureCollection('projects/nina/Europe_misc/CLMS_riparian_zone/rpz_DU003A_2018'))
  .merge(ee.FeatureCollection('projects/nina/Europe_misc/CLMS_riparian_zone/rpz_DU004A_2018'))
  .merge(ee.FeatureCollection('projects/nina/Europe_misc/CLMS_riparian_zone/rpz_DU005A_2018'))
  .merge(ee.FeatureCollection('projects/nina/Europe_misc/CLMS_riparian_zone/rpz_DU006A_2018'))
  .merge(ee.FeatureCollection('projects/nina/Europe_misc/CLMS_riparian_zone/rpz_DU007A_2018'))
  .merge(ee.FeatureCollection('projects/nina/Europe_misc/CLMS_riparian_zone/rpz_DU008A_2018'))
  .merge(ee.FeatureCollection('projects/nina/Europe_misc/CLMS_riparian_zone/rpz_DU009A_2018'))
  .merge(ee.FeatureCollection('projects/nina/Europe_misc/CLMS_riparian_zone/rpz_DU010A_2018'))
  .merge(ee.FeatureCollection('projects/nina/Europe_misc/CLMS_riparian_zone/rpz_DU011A_2018'))
  .merge(ee.FeatureCollection('projects/nina/Europe_misc/CLMS_riparian_zone/rpz_DU012A_2018'))
  .merge(ee.FeatureCollection('projects/nina/Europe_misc/CLMS_riparian_zone/rpz_DU013A_2018'))
  .merge(ee.FeatureCollection('projects/nina/Europe_misc/CLMS_riparian_zone/rpz_DU014A_2018'))
  .merge(ee.FeatureCollection('projects/nina/Europe_misc/CLMS_riparian_zone/rpz_DU015A_2018'))
  .merge(ee.FeatureCollection('projects/nina/Europe_misc/CLMS_riparian_zone/rpz_DU016A_2018'))
  .merge(ee.FeatureCollection('projects/nina/Europe_misc/CLMS_riparian_zone/rpz_DU017A_2018'))
  .merge(ee.FeatureCollection('projects/nina/Europe_misc/CLMS_riparian_zone/rpz_DU018A_2018'))
  .merge(ee.FeatureCollection('projects/nina/Europe_misc/CLMS_riparian_zone/rpz_DU019A_2018'))
  .merge(ee.FeatureCollection('projects/nina/Europe_misc/CLMS_riparian_zone/rpz_DU020A_2018'))
  .merge(ee.FeatureCollection('projects/nina/Europe_misc/CLMS_riparian_zone/rpz_DU021A_2018'))
  .merge(ee.FeatureCollection('projects/nina/Europe_misc/CLMS_riparian_zone/rpz_DU022A_2018'))
  .merge(ee.FeatureCollection('projects/nina/Europe_misc/CLMS_riparian_zone/rpz_DU023A_2018'))
  .merge(ee.FeatureCollection('projects/nina/Europe_misc/CLMS_riparian_zone/rpz_DU024A_2018'))
  .merge(ee.FeatureCollection('projects/nina/Europe_misc/CLMS_riparian_zone/rpz_DU025A_2018'))
  .merge(ee.FeatureCollection('projects/nina/Europe_misc/CLMS_riparian_zone/rpz_DU026A_2018'))
  .merge(ee.FeatureCollection('projects/nina/Europe_misc/CLMS_riparian_zone/rpz_DU027A_2018'))
  .merge(ee.FeatureCollection('projects/nina/Europe_misc/CLMS_riparian_zone/rpz_DU028A_2018'))
  .merge(ee.FeatureCollection('projects/nina/Europe_misc/CLMS_riparian_zone/rpz_DU029A_2018'))
  .merge(ee.FeatureCollection('projects/nina/Europe_misc/CLMS_riparian_zone/rpz_DU030A_2018'))
  .merge(ee.FeatureCollection('projects/nina/Europe_misc/CLMS_riparian_zone/rpz_DU031A_2018'))
  .merge(ee.FeatureCollection('projects/nina/Europe_misc/CLMS_riparian_zone/rpz_DU032A_2018'))
  .merge(ee.FeatureCollection('projects/nina/Europe_misc/CLMS_riparian_zone/rpz_DU033A_2018'))
  .merge(ee.FeatureCollection('projects/nina/Europe_misc/CLMS_riparian_zone/rpz_DU034A_2018'))
  .merge(ee.FeatureCollection('projects/nina/Europe_misc/CLMS_riparian_zone/rpz_DU035A_2018'))
  .merge(ee.FeatureCollection('projects/nina/Europe_misc/CLMS_riparian_zone/rpz_DU036A_2018'))
  .merge(ee.FeatureCollection('projects/nina/Europe_misc/CLMS_riparian_zone/rpz_DU037A_2018'))
  .merge(ee.FeatureCollection('projects/nina/Europe_misc/CLMS_riparian_zone/rpz_DU038A_2018'))
  .merge(ee.FeatureCollection('projects/nina/Europe_misc/CLMS_riparian_zone/rpz_DU039A_2018'))
  .merge(ee.FeatureCollection('projects/nina/Europe_misc/CLMS_riparian_zone/rpz_DU040A_2018'))
  .merge(ee.FeatureCollection('projects/nina/Europe_misc/CLMS_riparian_zone/rpz_DU041A_2018'))
  .merge(ee.FeatureCollection('projects/nina/Europe_misc/CLMS_riparian_zone/rpz_DU042A_2018'))
  .merge(ee.FeatureCollection('projects/nina/Europe_misc/CLMS_riparian_zone/rpz_DU043A_2018'));

//// Coastal ---------------------------------------------------
// Data from here: https://land.copernicus.eu/en/products/coastal-zones/coastal-zones-2018
var coastal = ee.FeatureCollection('projects/nina/Europe_misc/CLMS_euhydro_angerman_coastal_p_v013')
  .merge(ee.FeatureCollection('projects/nina/Europe_misc/CLMS_euhydro_angerman_transit_p_v013'));

/***
 * 2. Define strata -----------------------------------
 */
var riparianImg = ee.Image(0).paint(riparian, 1);
//riparianImg = riparianImg.updateMask(clcWater.eq(0).focal_max(100, 'circle', 'meters')).selfMask();
riparianImg = riparianImg.updateMask(clcWater.eq(0)).selfMask();

// Rasterize
var coastalImg = ee.Image(0).paint(coastal, 1);
Map.addLayer(coastalImg.selfMask(), {palette: ['#1b81c4']}, 'coastalImg raw', 0);
// Buffer by 1500m inland and 100m into water
coastalImg = coastalImg.focal_max(1500, 'circle', 'meters')
//coastalImg = coastalImg.updateMask(clcWater.eq(0).focal_max(100, 'circle', 'meters')).selfMask();
coastalImg = coastalImg.updateMask(clcWater.eq(0)).selfMask();

// Mask riparian zone
coastalImg = coastalImg.where(riparianImg, 0).selfMask();

// .reproject(greyTrendCol.first().projection().atScale(50))
Map.addLayer(coastalImg, {palette: ['#1b81c4']}, 'coastalImg', 0);
Map.addLayer(riparianImg, {palette: ['#19c58a']}, 'riparianImg', 0);

// Create strata image for generating samples - buffer included as single stratum
var strataImg = ee.Image(0)
  .where(clcWater.eq(0), 1) // inland stable
  .where(coastalImg, 2) // coastal stable
  .where(riparianImg, 3) // riparian stable
strataImg = strataImg.selfMask();

var palette = [
  // Inland group: yellow, orange, red
  '#FFFF00', // Yellow for inland stable (1)

  // Coastal group: light blue, medium blue, dark blue
  '#ADD8E6', // Light blue for coastal stable (4)

  // Riparian group: light green, medium green, dark green
  '#98FB98', // Light green for riparian stable (7)
];

Map.addLayer(strataImg.reproject(proj.atScale(50)), {min:1, max:3, palette:palette}, 'strataImg', 0)

var grid = ee.FeatureCollection('projects/nina/Arena/export_grid_EEA39')
Map.addLayer(grid, {}, 'grid', 0);

/***
 * 3. Generate exports ------------------------------------------------------------------
 */

var list= grid.toList(1000);

var list= grid.reduceColumns(ee.Reducer.toList(), ['CellCode']).get('list').evaluate(function(list){
  print(list)
  
  for (var i = 0; i<2; i++){
    var cellID = list[i];
    var feature = grid.filter(ee.Filter.eq('CellCode', cellID)).first();
    var aoi = feature.geometry();
    
    Export.image.toAsset({
      image: strataImg,
      region: aoi,
      description: 'ID_' + String(cellID),
      assetId: 'Arena/strataImg_riparian_coastal/ID_' + String(cellID),
      scale: 50,
      crs: 'EPSG:3035',
      maxPixels:1e13,
      pyramidingPolicy: 'mode'
    });
  }
  
})


