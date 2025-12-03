Map.setOptions('HYBRID')

// Here we will re-define strata because the pre-exported strata are at 50m resolution which 
// causes some irregularities in the distance transform image

var projCrs = 'EPSG:3035'
var proj = ee.Projection(projCrs).atScale(30);

/***
 * 1. Import the land cover and coastal/riparian maps to define strata -----------------------------------
 */
var clcplus = ee.Image('projects/nina/Europe_misc/CLMS_CLCplus_RASTER_2018_010m_eu_03035_V1_1');
clcplus = clcplus.remap(
  [1,2,3,4,5,6,7,8,9,10,11],
  [1,2,2,2,3,4,5,6,6, 7, 7]);
Map.addLayer(clcplus.randomVisualizer(), {}, 'clcplus', 0);

var clcWater = clcplus.eq(7);
Map.addLayer(clcWater, {min:0, max:1}, 'clcWater', 0)

var clcLand = clcplus.neq(7);
Map.addLayer(clcLand, {min:0, max:1}, 'clcLand', 0)


//// Coastal ---
// Data from here: https://land.copernicus.eu/en/products/coastal-zones/coastal-zones-2018
var coastal = ee.FeatureCollection('projects/nina/Europe_misc/CLMS_euhydro_angerman_coastal_p_v013')
  .merge(ee.FeatureCollection('projects/nina/Europe_misc/CLMS_euhydro_angerman_transit_p_v013'));


//// Riparian --
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

/***
 * 2. Define distance images for coastal and riparian--------------------------------------
 */
var coastlineImg = ee.Image(0).paint(coastal, 1).selfMask().reproject(proj);
Map.addLayer(coastlineImg, {min:0,max:1}, 'coastlineImg', 0)
var distCoast = coastlineImg.fastDistanceTransform(256).sqrt() 
        .multiply(ee.Image.pixelArea().sqrt()).reproject(proj);
distCoast = distCoast.updateMask(clcLand);
distCoast = distCoast.updateMask(distCoast.lt(10000))
Map.addLayer(distCoast, {min:0,max:2000, palette:['black', 'blue', 'white']}, 'distCoast', 0);


var riparianWater = riparian.filter(ee.Filter.eq('CODE_1_18', 8));
Map.addLayer(riparianWater, {}, 'riparianWater', 0)
var riparianImg = ee.Image(0).paint(riparianWater, 1)
  .setDefaultProjection(proj.atScale(5))
  .reduceResolution(ee.Reducer.max())
  .reproject(proj).selfMask();
Map.addLayer(riparianImg, {min:0,max:1}, 'riparianImg', 0)
var distFreshwater = riparianImg.fastDistanceTransform(256).sqrt() 
        .multiply(ee.Image.pixelArea().sqrt()).reproject(proj);
distFreshwater = distFreshwater.updateMask(clcLand);
distFreshwater = distFreshwater.updateMask(distFreshwater.lt(10000))
Map.addLayer(distFreshwater, {min:0,max:2000, palette:['black', 'blue', 'white']}, 'distFreshwater', 0);


var distImgExport = distCoast.rename('distCoast')
    .addBands(distFreshwater.rename('distWater'))
    .round().int();

var grid = ee.FeatureCollection('projects/nina/Arena/export_grid_EEA39')
Map.addLayer(grid, {}, 'grid', 0);

/***
 * 3. Generate exports --------------------------------------------------------------
 */
var list= grid.toList(1000);

var list= grid.reduceColumns(ee.Reducer.toList(), ['CellCode']).get('list').evaluate(function(list){
  print(list)
  
  for (var i = 0; i<2; i++){
    var cellID = list[i];
    var feature = grid.filter(ee.Filter.eq('CellCode', cellID)).first();
    var aoi = feature.geometry();
    
    Export.image.toAsset({
      image: distImgExport,
      region: aoi,
      description: 'ID_' + String(cellID),
      assetId: 'Arena/distImg_riparian_coastal/ID_' + String(cellID),
      scale: 30,
      crs: 'EPSG:3035',
      maxPixels:1e13,
      pyramidingPolicy: 'mean'
    });
  }
  
})