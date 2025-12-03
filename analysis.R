
library(raster)
library(tidyverse)
library(sf)
library(stringr)
library(gridExtra)
library(grid)
library(ggtext)
library(rnaturalearth)
library(ggpubr)
library(ggmagnify)
library(pals)

#### Import and prepare datasets ---------------------------------------------------------------
grid <- st_read('./data/export_grid_50km_landwater.shp') %>% st_transform(st_crs(3035))
ggplot() + geom_sf(data=grid)

bbox_poly <- st_as_sfc(st_bbox(grid), crs = st_crs(grid))
ggplot() + geom_sf(data=grid ) + geom_sf(data=bbox_poly, fill=NA)

countries <- ne_countries(scale = "medium", returnclass = "sf")%>% st_transform(st_crs(3035)) %>%
  st_filter(grid%>%st_union() %>% st_buffer(-100000)) %>%
  st_intersection(grid%>%st_union())
#countries <- st_read('./data/countries.geojson')%>% st_transform(st_crs(3035)) 

countriesShrunk <- countries %>%
  st_intersection(countries%>%st_union() %>% st_buffer(25000)%>% st_buffer(-40000))

ggplot()+ 
  #geom_sf(data=grid %>%st_union() %>% st_buffer(-50000)) +  
  geom_sf(data=countries, fill=NA)+  
  geom_sf(data=countries %>%st_union() %>% st_buffer(50000), fill=NA, color='red')+  
  geom_sf(data=countriesShrunk, fill=NA, color='green')

ecoregions <- st_read('./data/Ecoregions2017.shp') %>%
  st_transform(st_crs(grid)) %>%
  st_filter(grid) %>%
  dplyr::select(BIOME_NAME, ECO_NAME)%>%
  mutate(BIOME_NAME = ifelse(BIOME_NAME == 'Temperate Grasslands, Savannas & Shrublands',
                             'Temperate Broadleaf & Mixed Forests', BIOME_NAME))%>%
  mutate(BIOME_NAME = ifelse(BIOME_NAME == 'Mediterranean Forests, Woodlands & Scrub',
                             'Mediterranean Woodlands & Scrub', BIOME_NAME))
  
ecoregions
ecoregions %>% ggplot() + geom_sf(aes(fill=BIOME_NAME))

ecoregion_lookup <- grid %>%
  st_intersection(ecoregions) %>%
  mutate(area = as.numeric(st_area(geometry)) ) %>%
  st_drop_geometry() %>%
  group_by(id) %>%
  mutate(areaMax= max(area)) %>%
  filter(area == areaMax) %>%
  dplyr::select(id, BIOME_NAME, ECO_NAME)
length(unique(ecoregion_lookup$id))
nrow(ecoregion_lookup)
grid %>% left_join(ecoregion_lookup) %>% ggplot() + geom_sf(aes(fill=BIOME_NAME), color=NA)
grid %>% left_join(ecoregion_lookup) %>% ggplot() + geom_sf(aes(fill=ECO_NAME), color=NA)


clcLookup <- read_csv('./data/clc_lookup.csv') %>%
  dplyr::select(clc_class = `Map value`, 
                clc_1 = `CLC level 1`, 
                clc_3 = `CLC level 3`, 
                maes = `MAES ecosystem type`)
clcLookup

# Detailed level-3 CLC land cover areas for baseline and end-point in change pixels
clc_change_l3 <- read_csv('./data/from_gee/clc_areas_change_from_to_l3_2000_2018_grid_50km.csv')%>%
  mutate(
    landwater   = stratum %/% 524,
    clc_class = (stratum %% 524) 
  ) %>%
  filter(!change_code %in% c(14,15)) %>%
  mutate(change_code = recode_factor(factor(change_code), 
                                   "10" = "Loss to urban",
                                   "11" = "Gain from urban",
                                   "12" = "Loss to cropland",
                                   "13" = "Gain from cropland"),
         landwater = recode_factor(factor(landwater), 
                                   "1" = "Inland",
                                   "2" = "Coastal",
                                   "3" = "Riparian")) %>%
  left_join(clcLookup)%>%
  mutate(clc_base = str_split(change_code, ' ') %>% map_chr(3))%>%
  mutate(clc_lossgain = str_split(change_code, ' ') %>% map_chr(1)) 
hist(clc_change_l3$clc_class)  

# Change areas for simplified typology including baseline areas
clc_change <- read_csv('./data/from_gee/clc_areas_change_2000_2018_grid_50km.csv')%>%
  mutate(
    landwater   = stratum %/% 14, # see getStratAreas() function in GEE code baseMultiplier
    clc_class = (stratum %% 14) 
  ) %>%
  # Small area in CLC over France/Andora (1.6738, 42.5632) has no data - exclude this
  filter(clc_class != 0) %>%
  dplyr::select(-'system:index', -'.geo') %>%
  filter(landwater %in% c(1,2,3)) %>%
  mutate(clc_class = factor(clc_class),
         clc_class = recode_factor(clc_class,
                                   "1" = "Urban",
                                   "2" = "Cropland",
                                   "3" = "Grassland",
                                   "4" = "Woodland and forest",
                                   "5" = "Heathland and shrub",
                                   "6" = "Sparsely vegetated",
                                   "7" = "Wetlands",
                                   "8" = "Marine inlets",
                                   "9" = "Rivers and lakes",
                                   
                                   "10" = "Loss to urban",
                                   "11" = "Gain from urban",
                                   "12" = "Loss to cropland",
                                   "13" = "Gain from cropland"),
         landwater = recode_factor(factor(landwater), 
                                   "1" = "Inland",
                                   "2" = "Coastal",
                                   "3" = "Riparian")) %>%
  left_join(ecoregion_lookup, by = 'id')

plot(clc_change$landwater)
plot(clc_change$clc_class)

# Consumption and formation accounts per country
clc_cons_form <- read_csv(list.files('./data/from_gee/clc_areas_consumption_formation_2000_2018_countries/', full.names = T))%>%
  dplyr::select(country=id, label, area)
clc_cons_form %>%
  group_by(country, label) %>%
  summarise(area = sum(area))


# Change areas within darea# Change areas within distances of waters edge
clc_change_distance <- read_csv('./data/from_gee/clc_areas_change_distance_2000_2018_grid_50km.csv') %>%
  filter(!stratum %in% c(0)) %>%
  mutate(stratum = recode_factor(factor(stratum), 
                                 "1" = "nature",
                                 "2" = "cropland",
                                 "3" = "urban",
                                 "4" = "urban -> nature",
                                 "5" = "cropland -> nature",
                                 "6" = "nature -> urban",
                                 "7" = "nature -> cropland")) %>%
  mutate(type = ifelse(type == 'all', 'all water',
                       ifelse(type == 'coastal', 'coastal zone', 'riparian zone'))) %>%
  dplyr::select(id, type,stratum, distance, area) %>%
  mutate(distance = distance/1000)


# Raster images for visualization in R

clcChange_rast <- raster('./data/from_gee/clc18_maes_change_500m.tif')
plot(clcChange_rast)
clcChange_rast[clcChange_rast == 0] <- NA
plot(clcChange_rast)

clcChange_rast_1000m <- aggregate(clcChange_rast, 
                                  fact = 3, 
                                  fun = max, 
                                  na.rm = TRUE)

landwater_rast <- raster('./data/from_gee/landwater_500m.tif')
plot(landwater_rast)

landwater_rast[landwater_rast == 0] <- NA
plot(landwater_rast)
landwater_rast[landwater_rast == 2] <- 99
landwater_rast[landwater_rast == 3] <- 2
landwater_rast[landwater_rast == 99] <- 3

landwater_rast_1000m <- aggregate(landwater_rast, 
                                  fact = 3, 
                                  fun = max, 
                                  na.rm = TRUE)

#### Figure 1 - maps strata and gain/loss barplot ---------------------------------------------------
pal <- c('#a9384c',  '#4582bb','#f7de05')
pal <- c('#eadc91', '#4582bb', '#62c8ca')
dev.off()
toPlotDF <- as.data.frame(landwater_rast_1000m, xy=TRUE) %>% na.omit() %>%
  mutate(constant = factor(constant))
names(toPlotDF) <- c('Lon', 'Lat', 'value')
#hist(toPlotDF$Lat)
#hist(toPlotDF$Lon)
nrow(toPlotDF)
mapLandWater <- toPlotDF %>%
  ggplot()  +
  geom_sf(data = countries%>%st_union() %>% st_buffer(50000), fill='#d9d9d9', color=NA) +
  geom_tile(aes(x = Lon, y = Lat, fill = value)) +
  scale_fill_manual(values = pal, labels = c('Inland', 'Riparian', 'Coastal')) +
  geom_sf(data = countriesShrunk, fill = NA, alpha= 0.2,color = '#000000',size = 0.1) +
  coord_sf( expand = FALSE)+
  xlim(2590000,7370000) +
  ylim(1420000,5457000)+
  labs(title = 'a') +
  theme_void() +
  theme(legend.position =c(0.8, 0.7),
        legend.title = element_blank()) 
mapLandWater

pal2 <- c(
  '#556B2F', # 1 natural - dark olive green
  '#888d80', # 2 cropland - muted brownish yellow
  '#172b24', # 3 urban - dark reddish-brown
  '#00E5FF', # 4 gain from urban - bright cyan
  '#00FF99', # 5 gain from cropland - bright green
  '#FF4C4C', # 6 loss to urban - bright red
  '#e6f602'  # 7 loss to cropland - bright pink
)

toPlotDF_2 <- as.data.frame(clcChange_rast_1000m, xy=TRUE) %>% na.omit() %>%
  mutate(constant = factor(constant))
names(toPlotDF_2) <- c('Lon', 'Lat', 'value')

mapCLCchange <- toPlotDF_2 %>%
  ggplot()  +
  geom_sf(data = countries%>%st_union() %>% st_buffer(50000), fill='#d9d9d9', color=NA) +
  geom_tile(aes(x = Lon, y = Lat, fill = value)) +
  scale_fill_manual(values = pal2, labels = c('natural', 'cropland', 'urban',
                                              'urban -> natural', 'cropland -> natural',
                                              'natural -> urban', 'natural -> cropland')) +
  geom_sf(data = countries, fill = NA, alpha= 0.2,color = '#ffffff',size = 0.1) +
  coord_sf( expand = FALSE) +
  xlim(2590000,7370000) +
  ylim(1420000,5457000)+
  labs(title = 'b') +
  theme_void() +
  theme(legend.position =c(0.8, 0.7),
        legend.title = element_blank()) 
mapCLCchange


clc_change_l2 <- clc_change %>%
  mutate(clc_class = as.character(clc_class)) %>%
  # Terrestrial surfaces
  filter(!clc_class %in% c('Marine inlets', 'Rivers and lakes') ) %>%
  mutate(clc_class = recode_factor(factor(clc_class),
                                   "Urban" = "urban",
                                   "Cropland" = "cropland",
                                   "Grassland" = "nature",
                                   "Woodland and forest" = "nature",
                                   "Heathland and shrub" = "nature",
                                   "Sparsely vegetated" = "nature",
                                   "Wetlands" = "nature",
                                   
                                   "Loss to urban" = "nature -> urban",
                                   "Gain from urban" = "urban -> nature",
                                   "Loss to cropland" = "nature -> cropland",
                                   "Gain from cropland" = "cropland -> nature")) %>%
  group_by(clc_class) %>%
  summarise(area = sum(area)/1000000) %>%
  ungroup() %>%
  mutate(areaPerc = area / sum(area)*100)
clc_change_l2$clc_class <- factor(clc_change_l2$clc_class, levels = rev(c("nature" ,
                                                                       "cropland",
                                                                       "urban",
                                                                       "urban -> nature",
                                                                       "cropland -> nature",
                                                                       "nature -> urban",
                                                                       "nature -> cropland" )))

from <- c(ymin = 0.5, ymax = 4.2,  xmin = 0, xmax = 10000)
# Names xmin, xmax, ymin, ymax are optional:
to <- c(ymin = 0.7, ymax =3.7, xmin = 750000, xmax = 2750000)

plotAreaChange <- clc_change_l2 %>%
  mutate(label = ifelse(areaPerc < 0.1, paste0(round(areaPerc, 2), '%'), paste0(round(areaPerc, 1), '%'))) %>%
  ggplot(aes(y=clc_class, x = area, fill=clc_class)) +
  geom_bar(stat='identity') +
  geom_text(aes(label = label, x = area + (0*area)), size=2.5, hjust=-0.1) +
  scale_fill_manual(values=rev(pal2)) +
  #xlim(0, 4500000)+
  geom_magnify(from = from, to = to, axes = "x", shadow = TRUE, alpha =0.8, target.linetype=0) +
  theme(legend.position = 'none')  +
  labs(x = expression("Land area ("~km^2~")"),
       title = 'c') +
  theme(axis.title.y = element_blank())
plotAreaChange




# Urban expansion Turkiye
# https://earth.google.com/web/@37.13607752,27.55547983,13.59603518a,1308.63794699d,35y,-0h,0t,0r/data=CgwqBggBEgAYAUICCAE6AwoBMEICCABKDQj___________8BEAA

# Urba abandonment Germany
# https://earth.google.com/web/@53.92112117,10.85552885,156.58634283a,1165.64764158d,35y,0h,0t,0r/data=ChYqEAgBEgoyMDA2LTA1LTMxGAFCAggBOgMKATBCAggASg0I____________ARAA
# France
# https://earth.google.com/web/@43.76952619,-0.36625561,61.34489217a,1260.88903287d,35y,-0h,0t,0r/data=ChYqEAgBEgoyMDA5LTEwLTE4GAFCAggBOgMKATBCAggASg0I____________ARAA

# Crop expansion Romania
# https://earth.google.com/web/@47.31084134,22.41004056,134.0373039a,1516.21615602d,35y,0h,0t,0r/data=ChYqEAgBEgoyMDIyLTEwLTI0GAFCAggBOgMKATBCAggASg0I____________ARAA
# Spain
# https://earth.google.com/web/@41.06638769,-7.06053083,177.19634814a,4402.9150474d,35y,0h,0t,0r/data=ChYqEAgBEgoyMDAzLTA1LTIxGAFCAggBOgMKATBCAggASg0I____________ARAA
# https://earth.google.com/web/@0,0,0a,22251752.77375655d,35y,0h,0t,0r/data=CgRCAggBQgIIAEoNCP___________wEQAA

# Crop abandonment
# Hungray
# https://earth.google.com/web/@47.32504267,20.57438544,83.79475059a,2805.64580661d,35y,0h,0t,0r/data=ChYqEAgBEgoyMDAyLTA3LTE3GAFCAggBOgMKATBCAggASg0I____________ARAA
# Turkiye
# https://earth.google.com/web/@39.01058251,34.06291379,853.07672655a,3523.49685136d,35y,0h,0t,0r/data=ChYqEAgBEgoyMDIzLTA5LTIxGAFCAggBOgMKATBCAggASg0I____________ARAA
lab1 <- 'test'
makePngPlot <- function(i1, i2, head,  lab1, lab2){
  img1 <- png::readPNG(i1) 
  img2 <- png::readPNG(i2) 
  maxX <- max(iris$Sepal.Length)
  
  imgPlot1 <- ggplot(iris, aes(x = Sepal.Length/maxX*100, y = Sepal.Length/maxX*100))+
    geom_point(alpha=1) +
    background_image(img1) +
    annotate("text", x = 20, y = 100, size = 5,label = paste0('bold(',lab1,')'), color='white',parse = TRUE, hjust=-0.25) +
    coord_cartesian() +
    labs(title = head) +
    theme_void()
  #imgPlot1
  imgPlot2 <-  ggplot(iris, aes(x = Sepal.Length/maxX*100, y = Sepal.Length/maxX*100))+
    geom_point(alpha=1) +
    background_image(img2) +
    annotate("text", x = 20, y = 100, size = 5,label = paste0('bold(',lab2,')'), color='white',parse = TRUE, hjust=-0.25) +
    coord_cartesian() +
    labs(title = '') +
    theme_void()
  #imgPlot2
  
  outpng <- ggarrange(imgPlot1, imgPlot2, widths = c(1,1),ncol = 2, nrow = 1)
  return (outpng)
}
ex1 <- makePngPlot('./data/png/urb_exp_1_2004.png', './data/png/urb_exp_1_2024.png', 'd', '2004', '2024')
ex2 <- makePngPlot('./data/png/urb_aban_1_2006.png', './data/png/urb_aban_1_2024.png', 'e', '2006', '2024')
ex3 <- makePngPlot('./data/png/crop_exp_1_2009.png', './data/png/crop_exp_1_2019.png', 'f', '2009', '2019')
ex4 <- makePngPlot('./data/png/crop_aban_1_2002.png', './data/png/crop_aban_1_2018.png', 'g', '2002', '2018')

expanel <- grid.arrange(ex1, ex2, ex3, ex4, ncol=4, widths=c(1,1,1,1), padding = unit(0, "line"), newpage = F)

mappanel <- grid.arrange(mapLandWater,mapCLCchange,plotAreaChange, ncol=3, widths=c(1,1,1), padding = unit(0, "line"), newpage = F)

fig1 <- grid.arrange(mappanel,expanel, ncol=1, heights=c(2,1), padding = unit(0, "line"), newpage = F)

dev.off()
ggsave("fig1.png",fig1, width = 40, height=18, units='cm')


#### Habitat loss share and intensity by distance band ------------------------------------------------------------


clc_change_totals <- clc_change %>%
  mutate(stratum = clc_class) %>%
  # Terrestrial surfaces
  filter(!stratum %in% c('Marine inlets', 'Rivers and lakes') ) %>% 
  # group natural land cover
  mutate(stratum = if_else(!stratum %in% c('Urban', 'Cropland') & !str_detect(stratum, 'Gain|Loss'), 'Nature', stratum)) %>%
  group_by(stratum) %>%
  summarise(totArea = sum(area, na.rm=T)/1000000)
clc_change_totals

clc_dist_totals <- clc_change_distance %>%
  group_by(distance, type) %>%
  summarise(totArea = sum(area, na.rm=T)/1000000) %>%
  # Calculate band areas
  arrange(type, distance) %>%
  group_by(type) %>%
  mutate(totAreaBand = totArea - lag(totArea,1)) %>%
  mutate(totAreaBand = ifelse(is.na(totAreaBand), totArea, totAreaBand))

# Share of total landscape area within buffer distance
sp1 <- clc_change_distance%>%
  group_by(distance, stratum, type) %>%
  summarise(area = sum(area)/1000000)  %>%
  left_join(clc_dist_totals) %>%
  mutate(areaPerc = area/totArea*100) %>%
  mutate(clc_stablechange = ifelse(str_detect(stratum, 'Gain|Loss'), 'change', 'stable')) %>%
  ggplot(aes(x=distance, y = areaPerc, color=type)) +
  geom_point(alpha=0.25) + 
  geom_line() +
  scale_x_continuous(breaks=c(2,4,6,8,10), labels = paste('<', c(2,4,6,8,10))) +
  labs(title = 'a) land cover and habitat change: area share of buffer zone',
       color='Buffer type',
       x = "Buffer distance from water's edge (km)",
       y = "Percentage of land area \nwithin buffer (%)") + 
  facet_wrap(~ stratum, scales='free_y', nrow=1) +
  scale_color_manual(values = c('#3c0f73',  '#62c8ca','#4582bb'))+
  theme(legend.position='none')
sp1

# Share of total landscape inside buffer zone
# clc_change_distance%>%
#   group_by(distance, stratum, type) %>%
#   summarise(area = sum(area)/1000000)  %>%
#   # Calculate band areas
#   arrange(type, stratum, distance) %>%
#   group_by(type, stratum) %>%
#   mutate(areaBand = area - lag(area,1)) %>%
#   mutate(areaBand = ifelse(is.na(areaBand), area, areaBand)) %>%
#   left_join(clc_dist_totals) %>%
#   mutate(areaPerc = areaBand/totAreaBand*100) %>%
#   mutate(clc_stablechange = ifelse(str_detect(stratum, 'Gain|Loss'), 'change', 'stable')) %>%
#   ggplot(aes(x=distance, y = areaPerc, color=type)) +
#   geom_point(alpha=0.25) + 
#   geom_line() +
#   scale_x_continuous(breaks=c(2,4,6,8,10), labels = c('0-2', '2-4', '4-6', '6-8', '8-10')) +
#   labs(color='Buffer type',
#        x = "Distance buffer from water's edge (km)",
#        y = "Percentage of total land area (%)") + 
#   facet_wrap(~ stratum, scales='free_y', nrow=1) +
#   scale_color_manual(values = c('#3c0f73',  '#62c8ca','#4582bb'))



# Get baseline nature areas
clc_natural_dist_totals <- clc_change_distance %>%
  # Nature land cover
  # Including habitats lost since 2000 (i.e. they were nature in 2000)
  filter(stratum %in% c('nature') | str_detect(stratum,  'nature ->')) %>%
  group_by(distance, type) %>%
  summarise(totAreaNature = sum(area, na.rm=T)/1000000) %>%
  # Calculate band areas
  arrange(type, distance) %>%
  group_by(type) %>%
  mutate(totAreaNatureBand = totAreaNature - lag(totAreaNature,1)) %>%
  mutate(totAreaNatureBand = ifelse(is.na(totAreaNatureBand), totAreaNature, totAreaNatureBand))

# Create net loss intensity df
clc_netLoss_intensity_distance <-  clc_change_distance%>%
  filter(str_detect(stratum, '->')) %>%
  group_by(distance, stratum, type) %>%
  summarise(area = sum(area)/1000000) %>%
  mutate(clc_base = ifelse(str_detect(stratum, 'cropland'), 'cropland', 'urban'))%>%
  mutate(clc_lossgain = ifelse(str_detect(stratum, 'nature ->'), 'Loss', 'Gain'))%>%
  ungroup() %>%
  dplyr::select(distance,type,  clc_lossgain, clc_base, area) %>%
  pivot_wider(names_from=clc_lossgain, values_from=area)%>%
  # account for NA gain values - Tudnra
  mutate(Gain = ifelse(is.na(Gain), 0, Gain)) %>%
  mutate(netLoss = Loss - Gain,
         recoverPerc = Gain / Loss * 100)  %>%
  left_join(clc_natural_dist_totals) %>%
  mutate(netLossIntensity = netLoss/totAreaNature*100) %>%
  mutate(stratum = ifelse(clc_base == 'urban', '(nature -> urban) - (urban -> nature)','(nature -> cropland) - (cropland -> nature)' )) %>%
  dplyr::select(distance, stratum, type, areaIntensity = netLossIntensity)
clc_netLoss_intensity_distance

sp2 <- clc_change_distance%>%
  filter(str_detect(stratum, '>')) %>%
  group_by(distance, stratum, type) %>%
  summarise(area = sum(area)/1000000)  %>%
  left_join(clc_natural_dist_totals) %>%
  mutate(areaIntensity = area/totAreaNature*100) %>%
  bind_rows(clc_netLoss_intensity_distance) %>%
  mutate(stratum = factor(stratum, levels = c("urban -> nature",
                                              "cropland -> nature",
                                              "nature -> urban",
                                              "nature -> cropland",
                                              "(nature -> urban) - (urban -> nature)",
                                              "(nature -> cropland) - (cropland -> nature)"))) %>%
  ggplot(aes(x=distance, y = areaIntensity, color=type)) +
  geom_point(alpha=0.25) + 
  geom_line()+
  scale_x_continuous(breaks=c(2,4,6,8,10), labels = paste('<', c(2,4,6,8,10))) +
  labs(title = 'b) habitat change intensity: area share of baseline nature area within buffer zone',
       color='Buffer type',
       x = "Buffer distance from water's edge (km)",
       y = "Percentage of baseline nature\n area within buffer (%)") + 
  facet_wrap(~ stratum, scales='free_y', nrow=1,  labeller = label_wrap_gen(width = 24)) +
  scale_color_manual(values = c('#3c0f73',  '#62c8ca','#4582bb'))
sp2

fig2<- grid.arrange(sp1,sp2, ncol=1, heights=c(1,1), padding = unit(0, "line"), newpage = F)

dev.off()
ggsave("fig2.png",fig2, width = 40, height=18, units='cm')




# Supplement - share of total area over Europe
clc_perc_loss_distance <- clc_change_distance%>%
  group_by(distance, stratum, type) %>%
  summarise(area = sum(area)/1000000)  %>%
  bind_rows(tibble(distance = rep(rep(0,7),3), 
                   stratum = rep(unique(clc_change_distance$stratum), 3), 
                   type = rep(unique(clc_change_distance$type), 7), 
                   area = rep(rep(0,7), 3))) %>%
  left_join(clc_change_totals) %>%
  mutate(areaPerc = area/totArea*100) %>%
  mutate(clc_stablechange = ifelse(str_detect(stratum, 'Gain|Loss'), 'change', 'stable')) %>%
  mutate(stratum = factor(stratum, levels = c('Nature', 'Cropland', 'Urban', 'Gain from urban', 'Gain from cropland',
                                              'Loss to urban', 'Loss to cropland')))
clc_perc_loss_distance


View(clc_perc_loss_distance %>% filter(type == 'all') %>%
       filter(str_detect(stratum, 'Loss')) %>%
       group_by(distance) %>%
       summarise_at(vars(area, totArea), sum) %>%
       mutate(areaPerc = area/totArea*100))


clc_perc_loss_distance %>%
  ggplot(aes(x=distance, y = areaPerc, color=stratum)) +
  geom_point(alpha=0.25) + 
  geom_line() +
  scale_color_manual(values = c(
    '#647d39', # 1 natural - dark olive green
    '#888d80', # 2 cropland - muted brownish yellow
    '#172b24', # 3 urban - dark reddish-brown
    '#00E5FF', # 4 gain from urban - bright cyan
    '#00FF99', # 5 gain from cropland - bright green
    '#FF4C4C', # 6 loss to urban - bright red
    '#e6f602'  # 7 loss to cropland - bright pink
  )) +
  facet_wrap(~type, scales='free_y')


# Net change
clc_perc_loss_distance %>%
  ungroup() %>%
  filter(str_detect(stratum, 'Gain|Loss'))  %>%
  mutate(clc_base = str_split(stratum, ' ') %>% map_chr(3))%>%
  mutate(clc_lossgain = str_split(stratum, ' ') %>% map_chr(1)) %>%
  dplyr::select(type, clc_base,clc_lossgain, distance, area) %>%
  pivot_wider(names_from=clc_lossgain, values_from=area) %>%
  mutate(netLoss = Loss - Gain) %>%
  group_by(clc_base) %>%
  mutate(totNetLoss = sum(netLoss),
         netLossPerc = netLoss/totNetLoss*100) %>%
  ggplot(aes(x=distance, y = netLossPerc, color=type)) +
  geom_point() + 
  geom_line() +
  scale_x_continuous(breaks=c(2,4,6,8,10)) +
  facet_wrap(~ clc_base, scales='free_y', nrow=1)

#### Habitat loss, gain, net change grid level ------------------------------------------

clc_nature_grid_tot <- clc_change %>%
  # Conver to land and water zones for plotting
  mutate(landwater = ifelse(landwater == 'Inland', 'inland', 'water')) %>%
  # Terrestrial surfaces
  filter(!clc_class %in% c('Marine inlets', 'Rivers and lakes') ) %>% 
  # Nature land cover
  filter(!clc_class %in% c('Urban', 'Cropland')) %>%
  # Excluding habitats recovered since 2000 (i.e. they were urban or cropland in 2000)
  filter(!str_detect(clc_class, 'Gain')) %>%
  group_by(landwater, id) %>%
  summarise(totArea_landwater = sum(area, na.rm=T)/1000000) %>%
  ungroup() %>%
  mutate(percArea_landwater = totArea_landwater/sum(totArea_landwater)*100)
clc_nature_grid_tot



clc_change_grid_ratio <- clc_change %>% 
  # Conver to land and water zones for plotting
  mutate(landwater = ifelse(landwater == 'Inland', 'inland', 'water')) %>%
  filter(str_detect(clc_class, 'Loss|Gain')) %>%
  group_by(clc_class, landwater, id)%>%
  summarise(change_area = sum(area, na.rm=T)/1000000) %>%
  left_join(clc_nature_grid_tot, by = c('id', 'landwater')) %>%
  mutate(change_ratio = change_area / totArea_landwater * 100)%>%
  mutate(clc_base = str_split(clc_class, ' ') %>% map_chr(3))%>%
  mutate(clc_lossgain = str_split(clc_class, ' ') %>% map_chr(1))
clc_change_grid_ratio
colSums(is.na(clc_change_grid_ratio))

clc_netChange_grid_ratio <- clc_change_grid_ratio %>%
  ungroup() %>%
  dplyr::select(id, landwater, clc_lossgain, clc_base, change_area) %>%
  pivot_wider(names_from=clc_lossgain, values_from=change_area, values_fill = 0) %>%
  mutate(netChange = Gain - Loss,
         recoverPerc = Gain / Loss * 100) %>%
  left_join(clc_nature_grid_tot, by = c('id', 'landwater')) %>%
  mutate(change_ratio_netchange = netChange / totArea_landwater * 100,
         change_ratio_loss = Loss / totArea_landwater * 100,
         change_ratio_gain = Gain / totArea_landwater * 100)
clc_netChange_grid_ratio
colSums(is.na(clc_netChange_grid_ratio))

clc_netChange_grid_ratio %>%
  gather(key, val, change_ratio_netchange:change_ratio_gain) %>%
  ggplot(aes(x=val)) +
  geom_histogram() +
  facet_wrap(clc_base  ~key) +
  xlim(-20,20)



#https://www.joshuastevens.net/cartography/make-a-bivariate-choropleth-map/
#https://bluegreenlabs.org/post/map-building-3/
bivariate1 <- c('#3b4799', '#8a61af', '#bb63b1', 
                '#5a97bc', '#a5acd7', '#ddb0d9', 
                '#63c8c9', '#afe4e4', '#e8e8e8')
bivariate2 <- c('#3F2949', '#435786', '#4885C1', 
                '#77324C', '#806A8A', '#89A1C8', 
                '#AE3A4E', '#BC7C8F', '#CABED0')
bivariate3 <- c("#e8e8e8", "#e4d9ac", "#c8b35a", 
                "#cbb8d7", "#c8ada0","#af8e53", 
                "#9972af", "#976b82", "#804d36")
bivariate4 <- c("#e8e8e8", "#b8d6be", "#73ae80",
                "#b5c0da", "#90b2b3", "#5a9178",
                "#6c83b5", "#567994", "#2a5a5b")
bivariate5 <- c("#804d36","#e8e8e8",'#73ae80',
                "#e8e8e8","#e8e8e8","#e8e8e8",
                "#6c83b5", "#e8e8e8","#2a5a5b")

#https://jakubnowosad.com/posts/2020-08-25-cbc-bp2/
# https://cran.r-project.org/web/packages/pals/vignettes/pals_examples.html


responseVar <- 'change_ratio_netloss'
landuse <- 'urban'

varX <- 'water'
varY <- 'inland'

labelX <- 'water'
labelY <- 'inland'
title <- 'test title'
percMin <- 0.1
percMax <- 0.99

bivPal <- bivariate1
logTrans <- FALSE

makeBiVarMap <- function(responseVar, landuse,  logTrans, center, percMin, percMax, title, subtitle, bivPal, fillVal){
  varX <- 'water'
  varY <- 'inland'
  labelX <- 'land-water'
  labelY <- 'inland'
  
  bivSel <- bivPal
  legend_3 <- tibble(
    "3 - 3" = bivSel[1],
    "2 - 3" = bivSel[2],
    "1 - 3" = bivSel[3],
    "3 - 2" = bivSel[4],
    "2 - 2" = bivSel[5], 
    "1 - 2" = bivSel[6],
    "3 - 1" = bivSel[7],
    "2 - 1" = bivSel[8],
    "1 - 1" = bivSel[9]
  ) %>%
    gather("group", "fill")
  
  df <- clc_netChange_grid_ratio %>%
    filter(clc_base == landuse) %>%
    mutate(response = .[[responseVar]]) %>%
    dplyr::select(id, landwater, response) %>%
    pivot_wider(names_from=landwater, values_from=response, values_fill = 0) 
  if (logTrans){
    df <- df%>%
      mutate_at(vars(varX, varY), function(x){log(x+0.0001)})
  }
  hist(df$inland)
  hist(df$water)
  
  datBiVarToPlot <- df %>% 
    mutate(x =df[[varX]],
           y = df[[varY]]) %>%
    dplyr::select(id, x, y) 
  
  limsY <- quantile(datBiVarToPlot$y, probs=c(percMin, percMax))
  
  if (center){
    interRange <- (limsY[2] - limsY[1])/2
    limsY <- c(-interRange, interRange)
  }
  
  #limsY <- c(min(datBiVarToPlot$y), max(datBiVarToPlot$y))
  breaksY <- seq(limsY[1], limsY[2], (limsY[2]-limsY[1])/6)
  labsY <- round(c(breaksY[2], breaksY[4], breaksY[6]), 2)
  
  
  if (logTrans){
    labsY <- round(exp(c(breaksY[2], breaksY[4], breaksY[6])), 2)
  }
  legBreaksY <- c(breaksY[2], breaksY[4], breaksY[6])
  
  # limsX <- quantile(datBiVarToPlot$x, probs=c(0.01, 0.99))
  # limsX <- c(min(datBiVarToPlot$x), max(datBiVarToPlot$x))
  # breaksX <- seq(limsX[1], limsX[2], (limsX[2]-limsX[1])/6)
  # labsX <- round(c(breaksX[2], breaksX[4], breaksX[6]))
  # labsX <- round(exp(c(breaksX[2], breaksX[4], breaksX[6])), 3)
  # legBreaksX <- c(breaksX[2], breaksX[4], breaksX[6])
  
  limsX <- limsY
  breaksX <- breaksY
  labsX <- labsY
  legBreaksX <- legBreaksY
  
  
  g1 <- legend_3 %>%
    separate(group,
             into = c(varY, varX),
             sep = " - ") %>%
    mutate_at(vars(varX), function(varX){dplyr::recode(varX, "1" = legBreaksX[1], "2" = legBreaksX[2], "3" = legBreaksX[3])}) %>%
    mutate_at(vars(varY), function(varY){dplyr::recode(varY, "1" = legBreaksY[1], "2" = legBreaksY[2], "3" = legBreaksY[3])}) %>%
    ggplot() +
    geom_tile(mapping = aes(x =.data[[varX]],y = .data[[varY]],  fill = fill)) +
    scale_fill_identity() +
    labs(x = paste0(labelX, '  →'), y = paste0(labelY, '  →')) +
    theme(axis.title = element_text(size = 11),
          axis.text = element_text(size=8)) +
    scale_y_continuous(oob=scales::squish,  
                       limits=c(limsY[1],limsY[2]), 
                       labels = labsY,
                       breaks=legBreaksY) +
    scale_x_continuous(oob=scales::squish,
                       limits=c(limsX[1],limsX[2]), 
                       labels = labsX,
                       breaks=legBreaksX) +
    #geom_point(data=datBiVarToPlot,aes(x=x, y=y), alpha=0.2, size=0.3) +
    #geom_smooth(data=datBiVarToPlot,aes(x=x, y=y),method='lm', 
     #           color='black', se=F, linetype=2, alpha=0.6, size=0.5) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(fill=NA))
  g1
  
  if (center){
    g1 <- g1 + labs(x = paste0('← ', labelX, ' →'), y = paste0('← ', labelY, '→'))
  }
  
  
  datMap <- datBiVarToPlot  %>%
    mutate_at(vars(x), function(x){ifelse(x <= breaksX[3], "1",
                                          ifelse(x >= breaksX[5], "3",
                                                 "2"))}) %>%
    mutate_at(vars(y), function(y){ifelse(y <= breaksY[3], "1",
                                          ifelse(y >= breaksY[5], "3",
                                                 "2"))})  %>%
    mutate(
      group = paste(y, x, sep = " - ")
    )
  
  datMap <- left_join(datMap, legend_3)
  
  toMap <- grid %>%
    left_join(datMap) %>%
    mutate(fill = ifelse(is.na(fill), bivPal[9], fill))
  
  m1 <- ggplot() +
    geom_sf(data = countries, fill = "#e7e8ea", alpha= 0.2,color = '#172b24',size = 0.1)+
    geom_sf(data = toMap,  aes(fill=fill),  color=NA, alpha=0.7) +
    scale_fill_identity() + 
    theme_void() +
    labs(title = title,
         subtitle = subtitle) +
    xlim(2590000,7370000) +
    ylim(1420000,5457000)
  m1
  
  grob <- ggplotGrob(g1)
  b <- st_bbox(st_buffer(countries, -50000))
  outPlot <- m1 + annotation_custom(grob = grob, 
                                    xmin=7370000*0.75, 
                                    xmax=7370000*0.95, 
                                    ymin=5457000*0.7, 
                                    ymax=5457000 *0.95)
  outPlot
  #outPlot <- grid.arrange( g1, m1, nrow=1, padding = unit(0, "line"),widths=c(1,1), newpage = T)
  
  return (outPlot)
  
}


bv1 <-makeBiVarMap('change_ratio_loss', 'urban', TRUE, FALSE,0.1, 1, 'a) Habitat loss urban expansion', 'nature -> urban', rev(bivariate3))
bv1
bv2 <-makeBiVarMap('change_ratio_gain', 'urban', TRUE,FALSE,0.6, 0.99, 'b) Habitat gain urban abandonment', 'urban -> nature', rev(bivariate4))
bv3 <-makeBiVarMap('change_ratio_netchange', 'urban', FALSE,TRUE, 0.1, 0.9,'c) Habitat net change (urban)',  'nature <-> urban', rev(bivariate5))
bv3

bv4 <-makeBiVarMap('change_ratio_loss', 'cropland',TRUE,FALSE, 0.3, 0.99, 'd) Habitat loss cropland expansion', 'nature -> cropland', rev(bivariate3))
bv4
bv5 <-makeBiVarMap('change_ratio_gain', 'cropland', TRUE,FALSE, 0.3, 1,'e) Habitat gain cropland abandonment',  'cropland -> nature', rev(bivariate4))
bv6 <-makeBiVarMap('change_ratio_netchange', 'cropland',FALSE,TRUE, 0.1, 0.9,'f) Habitat net change (cropland)',  'nature <-> cropland', rev(bivariate5))
bv6

fig3 <- grid.arrange(bv1,bv2,bv3,bv4, bv5, bv6,  ncol=3, heights=c(1,1), padding = unit(0, "line"), newpage = F)

dev.off()
ggsave("fig3.png",fig3, width = 40, height=25, units='cm')

#### Habitat loss, gain, net change EU-level  -----------------------------------------------------------------------------


# Iterate over biomes to get conversion intensity ratios
biomes <- clc_change %>% drop_na(BIOME_NAME) %>% filter(BIOME_NAME != 'N/A') %>% 
  group_by(BIOME_NAME) %>%
  summarise() %>%pull(BIOME_NAME)
biomes <- c("All" ,biomes)
biomes

clc_netChange_ratio_all <- tibble()

for (b in biomes){
  
  if (b == 'All'){
    clc_change_filt <- clc_change
  } else {
    clc_change_filt <- clc_change %>% filter(BIOME_NAME == b)
  }
  
  clc_nature_tot <- clc_change_filt %>%
    # Terrestrial surfaces
    filter(!clc_class %in% c('Marine inlets', 'Rivers and lakes') ) %>% 
    # Nature land cover
    filter(!clc_class %in% c('Urban', 'Cropland')) %>%
    # Excluding habitats recovered since 2000 (i.e. they were urban or cropland in 2000)
    filter(!str_detect(clc_class, 'Gain')) %>%
    group_by(landwater) %>%
    summarise(totArea_landwater = sum(area, na.rm=T)/1000000) %>%
    ungroup() %>%
    mutate(percArea_landwater = totArea_landwater/sum(totArea_landwater)*100)
  clc_nature_tot
  
  clc_change_summary <- clc_change_filt %>% 
    filter(str_detect(clc_class, 'Loss|Gain')) %>%
    group_by(clc_class, landwater)%>%
    summarise(change_area = sum(area, na.rm=T)/1000000) %>%
    left_join(clc_nature_tot) %>%
    mutate(clc_base = str_split(clc_class, ' ') %>% map_chr(3))%>%
    mutate(clc_lossgain = str_split(clc_class, ' ') %>% map_chr(1))
  clc_change_summary
  
  
  clc_netChange_ratio <- clc_change_summary %>%
    ungroup() %>%
    dplyr::select(landwater, clc_lossgain, clc_base, change_area) %>%
    pivot_wider(names_from=clc_lossgain, values_from=change_area) %>%
    # account for NA gain values - Tudnra
    mutate(Gain = ifelse(is.na(Gain), 0, Gain)) %>%
    mutate(netLoss = Loss - Gain,
           recoverPerc = Gain / Loss * 100) %>%
    left_join(clc_nature_tot) %>%
    mutate(change_ratio_netloss = netLoss / totArea_landwater * 100,
           change_ratio_loss = Loss / totArea_landwater * 100,
           change_ratio_gain = Gain / totArea_landwater * 100)
  clc_netChange_ratio
  
  clc_netChange_ratio_all <- clc_netChange_ratio_all %>% 
    bind_rows(clc_netChange_ratio %>%
                mutate(biome = b))
  
  
}

clc_netChange_ratio_all

colSums(is.na(clc_netChange_ratio_all))

clc_netChange_ratio_all %>%
  gather(key, val, change_ratio_netloss:change_ratio_gain) %>%
  ggplot(aes(x=landwater, y = val, color=key)) +
  geom_point() +
  coord_flip() +
  facet_grid(biome~clc_base) +
  theme()

clc_netChange_ratio_all %>%
  ggplot(aes(x=landwater, y = change_ratio_netloss*-1, fill=change_ratio_netloss*-1)) +
  geom_hline(yintercept = 0) +
  #geom_col(aes(y=change_ratio_loss*-1), alpha=0.5) +
  geom_segment(inherit.aes=F,
               aes(x=landwater,xend=landwater,y= 0, yend=change_ratio_loss*-1), 
               arrow=arrow(length = unit(0.15, "cm"), type = "open")) +
  #geom_col(aes(y=change_ratio_gain), alpha=0.5) +
  geom_segment(inherit.aes=F,
               aes(x=landwater,xend=landwater,y= 0, yend=change_ratio_gain), 
               color='#6fdbde', arrow=arrow(length = unit(0.15, "cm"), type = "open")) +
  geom_col(width=0.5, alpha=0.5) +
  #geom_point() +
  coord_flip() +
  facet_grid(biome~clc_base, labeller = label_wrap_gen(width = 10)) +
  scale_fill_gradientn(colors=c('#bb63b1',  '#6c83b5'), limits=c(-1,1), oob=scales::squish) +
  theme(legend.position = 'none')


gridToMap <- grid%>%
  mutate(lon =st_coordinates(st_centroid(geometry))[,1]) %>%
  filter(lon > 2250000)

biomeselect <- 'Tundra'
title <- 'a)'
xlims <- c(-1,0.5)
xaxis <- FALSE
makeChangePlot <- function(biomeselect, title, xlims, legend, xaxis){
  
  pLeft <- clc_netChange_ratio_all %>%
    filter(biome == biomeselect) %>%
    filter(clc_base == 'urban') %>%
    ggplot(aes(x=landwater, y = change_ratio_netloss*-1, fill=change_ratio_netloss*-1)) +
    geom_hline(yintercept = 0) +
    geom_segment(inherit.aes=F,
                 aes(x=landwater,xend=landwater,y= 0, yend=change_ratio_loss*-1), 
                 arrow=arrow(length = unit(0.15, "cm"), type = "open")) +
    geom_segment(inherit.aes=F,
                 aes(x=landwater,xend=landwater,y= 0, yend=change_ratio_gain), 
                 color='#6fdbde', arrow=arrow(length = unit(0.15, "cm"), type = "open")) +
    geom_col(width=0.35, alpha=0.5) +
    ylim(xlims) +
    coord_flip() +
    facet_grid(.~clc_base) +
    labs(title = paste0(title, ' ', biomeselect),
         y = 'Habitat gain/loss due to urban \nchange (% of baseline area)') +
    scale_fill_gradientn(colors=c('#bb63b1',  '#6c83b5'), limits=c(-1,1), oob=scales::squish) +
    theme() +
    theme(
      plot.title = element_text(size=9),
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      axis.title.x = element_text(size = 8),
      axis.ticks.y = element_blank(),
      strip.background = element_blank(), # Removes the background of the facet strip
      strip.text = element_blank()        # Removes the text labels within the facet strip
    ) 
  
  if (legend) {
    pLeft <- pLeft+
      # Dummy legend
      geom_line(aes(color = "Gross loss"), alpha = 0) +
      geom_line(aes(color = "Gross gain"), alpha = 0) +
      geom_line(aes(color = "Net change"), alpha = 0) +
      scale_color_manual(values = c("Gross loss" = "black", "Gross gain" = "#6fdbde", "Net change" = "#bb63b1"), # Assign desired color
                         breaks = c("Gross loss","Gross gain","Net change")) +
      guides(fill='none',color = guide_legend(override.aes = list(alpha = 1, linewidth = 2))) +
      theme(legend.title= element_blank(),
            legend.background = element_blank(),
            legend.position = c(0.2, 0.5))
  } else {
    pLeft <- pLeft + theme(legend.position = 'none')
  }
  pLeft
  
  pRight <- clc_netChange_ratio_all %>%
    filter(biome == biomeselect) %>%
    filter(clc_base == 'cropland') %>%
    ggplot(aes(x=landwater, y = change_ratio_netloss*-1, fill=change_ratio_netloss*-1)) +
    geom_hline(yintercept = 0) +
    geom_segment(inherit.aes=F,
                 aes(x=landwater,xend=landwater,y= 0, yend=change_ratio_loss*-1), 
                 arrow=arrow(length = unit(0.15, "cm"), type = "open")) +
    geom_segment(inherit.aes=F,
                 aes(x=landwater,xend=landwater,y= 0, yend=change_ratio_gain), 
                 color='#6fdbde', arrow=arrow(length = unit(0.15, "cm"), type = "open")) +
    geom_col(width=0.35, alpha=0.5) +
    ylim(xlims) +
    coord_flip() +
    facet_grid(.~clc_base) +
    labs(title = '',
         y = 'Habitat gain/loss due to cropland \nchange (% of baseline area)') +
    scale_fill_gradientn(colors=c('#bb63b1',  '#6c83b5'), limits=c(-1,1), oob=scales::squish) +
    theme() +
    theme(
      legend.position = 'none',
      axis.title.y = element_blank(),
      axis.title.x = element_text(size = 8),
      axis.text.y = element_text(hjust = 0),
      axis.ticks.y = element_blank(),
      strip.background = element_blank(), # Removes the background of the facet strip
      strip.text = element_blank()        # Removes the text labels within the facet strip
    )
  pRight
  
  if (!xaxis){
    pLeft <- pLeft + theme(axis.title.x = element_blank(), axis.text.x = element_blank()) 
    pRight <- pRight + theme(axis.title.x = element_blank(), axis.text.x = element_blank())
  }
  
  biomeToMap <- if(biomeselect == 'All'){
    gridToMap
  } else {
    gridToMap%>%
      left_join(ecoregion_lookup) %>% 
      filter(BIOME_NAME == biomeselect)
  }
  
  pm <- biomeToMap %>%
    ggplot() + 
    geom_sf(data = gridToMap %>% st_union(), color=NA, fill='#7f7f7f') +
    geom_sf(fill='#f6978f', color=NA) +
    theme_void()
  
  pRight <- pRight +annotation_custom(grob=ggplotGrob(pm), 
                            ymin = -1.15, ymax=-0.45, xmin=0.5, xmax=3.5)
  
  p <-  grid.arrange(pLeft, pRight, ncol=2, widths=c(1,1.2), padding = unit(0, "line"), newpage = F)
  
  return (p)
  
}

biomes

cp1 <- makeChangePlot('All', 'a)', c(-1.2,0.6), FALSE, FALSE)
cp1
cp2 <- makeChangePlot('Tundra', 'b)', c(-1.2,0.6), TRUE, FALSE)
cp3 <- makeChangePlot('Boreal Forests/Taiga', 'c)', c(-1.2,0.6), FALSE, FALSE)
cp4 <- makeChangePlot('Temperate Broadleaf & Mixed Forests', 'd)', c(-1.2,0.6),FALSE,  FALSE)
cp4
cp5 <- makeChangePlot('Temperate Conifer Forests', 'e)', c(-1.2,0.6),FALSE,  FALSE)
cp6 <- makeChangePlot('Mediterranean Woodlands & Scrub', 'f)', c(-1.2,0.6), FALSE, TRUE)
cp6
fig4 <- grid.arrange(cp1, cp2, cp3, cp4, cp5, cp6, ncol=1, heights=c(1,1,1,1,1,1.2), padding = unit(0, "line"), newpage = F)

dev.off()
ggsave("fig4.png",fig4, width = 15, height=20, units='cm')

grid %>% left_join(ecoregion_lookup) %>% ggplot() + geom_sf(aes(fill=BIOME_NAME), color=NA)

View(clc_netChange_ratio_all %>%
  filter(biome == 'All') %>%
  group_by(landwater) %>%
  summarise(netLoss = sum(netLoss),
            grossLoss = sum(Loss),
            totArea_landwater = first(totArea_landwater)) %>%
  mutate(change_ratio_netloss = netLoss / totArea_landwater * 100,
         change_ratio_grossloss = grossLoss / totArea_landwater * 100) %>%
  ungroup() %>%
  mutate(totNetLoss = sum(netLoss),
         totGrossLoss = sum(grossLoss),
         totGain = totGrossLoss - totNetLoss,
         percNetLoss = netLoss/totNetLoss*100,
         percBaseline = totArea_landwater/sum(totArea_landwater)*100))
0.551/0.198
0.239/0.198



#### Baseline and end-point land cover shares of change ----------------------------------


clc_change_l3 %>%
  mutate(clc_base = str_split(change_code, ' ') %>% map_chr(3))%>%
  mutate(clc_lossgain = str_split(change_code, ' ') %>% map_chr(1)) %>%
  group_by(clc_base, clc_lossgain, clc_3) %>%
  summarise(area = sum(area)/1000000) %>%
  group_by(clc_base, clc_lossgain) %>%
  mutate(areaTot = sum(area),
         areaPerc = area/areaTot*100) %>%
  ggplot(aes(x=clc_lossgain, y = areaPerc, fill=clc_3)) +
  geom_col()+
  geom_text(aes(label = clc_3)) +
  facet_wrap(~clc_base) +
  theme(legend.position = 'none')

makeChangeDriverPlot <- function(){
  clc_change_l3_perc <- clc_change_l3%>%
    group_by(landwater, clc_lossgain, clc_3) %>%
    summarise(area = sum(area)/1000000) %>%
    group_by(landwater, clc_lossgain) %>%
    mutate(areaTot = sum(area),
           areaPerc = area/areaTot*100) %>%
    filter(areaPerc > 1) %>%
    mutate(clc_3 = substr(clc_3, 1, 42)) %>%
    group_by(clc_lossgain, clc_3) %>%
    mutate(ordering = sum(areaPerc))
  #library(ggfittext)
  cd1 <- clc_change_l3_perc %>%
    filter(clc_lossgain == 'Loss')   %>%
    ggplot(aes(x=reorder(clc_3, ordering), y = areaPerc, fill=landwater)) +
    geom_col(alpha=0.8, position = position_dodge(width=0.75,preserve = "single"), width=0.75, color='white') +
    coord_flip() + 
    #geom_bar_text(inherit.aes=F, aes(x=reorder(clc_3, ordering), label = clc_3, y = 0), 
    #              colour='black', reflow=T) +
    #ylim(-50,100) +
    scale_x_discrete(labels = scales::label_wrap(width = 25)) +
    scale_fill_manual(values = c('#eadc91', '#62c8ca', '#4582bb')) +
    scale_color_manual(values = c('#eadc91',  '#62c8ca','#4582bb')) +
    labs(x = 'CLC level 3 land use category',
         y = 'Area percentage',
         title = 'a) Post-loss land cover') +
    theme(legend.position = c(0.7, 0.3),
          axis.title.y = element_blank(),
          axis.text.y = element_text(size=8),
          legend.title = element_blank(),
          #panel.grid.major.y = element_line( size=.6, color="red" ),
          legend.background = element_blank())
  cd1
  
  cd2 <- clc_change_l3_perc %>%
    filter(clc_lossgain == 'Gain')   %>%
    ggplot(aes(x=reorder(clc_3, ordering), y = areaPerc, fill=landwater)) +
    geom_col(alpha=0.8, position = position_dodge(width=0.75,preserve = "single"), width=0.75, color='white') +
    coord_flip() + 
    scale_x_discrete(labels = scales::label_wrap(width = 25)) +
    scale_fill_manual(values = c('#eadc91', '#62c8ca', '#4582bb')) +
    scale_color_manual(values = c('#eadc91',  '#62c8ca','#4582bb')) +
    labs(x = 'CLC level 3 land use category',
         y = 'Area percentage',
         title = 'b) Post-recovery land cover') +
    theme(legend.position = 'none',
          axis.title.y = element_blank(),
          axis.text.y = element_text(size=8),
          legend.title = element_blank(),
          legend.background = element_blank())
  cd2
  fig5 <- grid.arrange(cd1, cd2, ncol=2, widths=c(1,1), padding = unit(1, "lines"), newpage = F)
  
  return (fig5)
  
}
makeChangeDriverPlot()

ggsave("fig5.png",makeChangeDriverPlot(), width = 30, height=12, units='cm')


