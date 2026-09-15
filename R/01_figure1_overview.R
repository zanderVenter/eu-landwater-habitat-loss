#### 01 - Figure 1: strata map, CLC change map, area summary, example panels
#
# Run from the repo root: Rscript R/01_figure1_overview.R
# Depends on: R/00_setup.R (sourced below).
# Writes: <OUTPUTS_DIR>/fig1.png
#
# Panels: (a) coastal/riparian/inland strata map, (b) CLC 2018 status +
# change map, (c) land area bar chart by class, (d-g) before/after PlanetScope
# example image pairs for urban/cropland expansion/abandonment.
#
# Author: Zander Venter

source("R/00_setup.R")
library(ggmagnify) # remotes::install_github("hughjonesd/ggmagnify")
# ggmagnify's shadow=TRUE (used below) needs the ggfx package installed
# (install.packages("ggfx")) - not explicitly library()'d, ggmagnify calls
# it internally.

# The original script used ggpubr::background_image()/ggarrange() here, but
# those pull in ggpubr's full car/rstatix/lme4 dependency chain (which needs
# a system libnlopt) for two functions with trivial equivalents already in
# packages this pipeline depends on elsewhere:
#   background_image(img) == annotation_raster(img, -Inf, Inf, -Inf, Inf)  (ggplot2)
#   ggarrange(p1, p2, ncol=2, nrow=1, widths=c(1,1)) == the same call to
#     gridExtra::grid.arrange(), already used throughout this file.
# Avoids the system dependency entirely rather than working around it.

#### Panel a: strata map ---------------------------------------------------
pal <- c('#eadc91', '#4582bb', '#62c8ca')

toPlotDF <- as.data.frame(landwater_rast_1000m, xy=TRUE) %>% na.omit() %>%
  mutate(constant = factor(constant))
names(toPlotDF) <- c('Lon', 'Lat', 'value')

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

#### Panel b: CLC status + change map ---------------------------------------
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

#### Panel c: land area bar chart -------------------------------------------
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
to <- c(ymin = 0.7, ymax =3.7, xmin = 750000, xmax = 2750000)

plotAreaChange <- clc_change_l2 %>%
  mutate(label = ifelse(areaPerc < 0.1, paste0(round(areaPerc, 2), '%'), paste0(round(areaPerc, 1), '%'))) %>%
  ggplot(aes(y=clc_class, x = area, fill=clc_class)) +
  geom_bar(stat='identity') +
  geom_text(aes(label = label, x = area + (0*area)), size=2.5, hjust=-0.1) +
  scale_fill_manual(values=rev(pal2)) +
  geom_magnify(from = from, to = to, axes = "x", shadow = TRUE, alpha =0.8, target.linetype=0) +
  theme(legend.position = 'none')  +
  labs(x = expression("Land area ("~km^2~")"),
       title = 'c') +
  theme(axis.title.y = element_blank())

#### Panels d-g: before/after example image pairs ---------------------------
# Coordinates for manual inspection in Google Earth / Esri Wayback (kept for
# provenance - these are where the example PNGs below were captured from):
# Urban expansion Turkiye: https://earth.google.com/web/@37.13607752,27.55547983,13.59603518a,1308.63794699d,35y,-0h,0t,0r
# Urban abandonment Germany: https://earth.google.com/web/@53.92112117,10.85552885,156.58634283a,1165.64764158d,35y,0h,0t,0r
# Crop expansion Romania: https://earth.google.com/web/@47.31084134,22.41004056,134.0373039a,1516.21615602d,35y,0h,0t,0r
# Crop abandonment Hungary: https://earth.google.com/web/@47.32504267,20.57438544,83.79475059a,2805.64580661d,35y,0h,0t,0r

makePngPlot <- function(i1, i2, head, lab1, lab2){
  img1 <- png::readPNG(i1)
  img2 <- png::readPNG(i2)
  maxX <- max(iris$Sepal.Length)

  imgPlot1 <- ggplot(iris, aes(x = Sepal.Length/maxX*100, y = Sepal.Length/maxX*100))+
    geom_point(alpha=1) +
    annotation_raster(img1, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
    annotate("text", x = 20, y = 100, size = 5,label = paste0('bold(',lab1,')'), color='white',parse = TRUE, hjust=-0.25) +
    coord_cartesian() +
    labs(title = head) +
    theme_void()
  imgPlot2 <-  ggplot(iris, aes(x = Sepal.Length/maxX*100, y = Sepal.Length/maxX*100))+
    geom_point(alpha=1) +
    annotation_raster(img2, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
    annotate("text", x = 20, y = 100, size = 5,label = paste0('bold(',lab2,')'), color='white',parse = TRUE, hjust=-0.25) +
    coord_cartesian() +
    labs(title = '') +
    theme_void()

  outpng <- grid.arrange(imgPlot1, imgPlot2, ncol = 2, nrow = 1, widths = c(1,1))
  return (outpng)
}
ex1 <- makePngPlot(data_path('png/urb_exp_1_2004.png'), data_path('png/urb_exp_1_2024.png'), 'd', '2004', '2024')
ex2 <- makePngPlot(data_path('png/urb_aban_1_2006.png'), data_path('png/urb_aban_1_2024.png'), 'e', '2006', '2024')
ex3 <- makePngPlot(data_path('png/crop_exp_1_2009.png'), data_path('png/crop_exp_1_2019.png'), 'f', '2009', '2019')
ex4 <- makePngPlot(data_path('png/crop_aban_1_2002.png'), data_path('png/crop_aban_1_2018.png'), 'g', '2002', '2018')

expanel <- grid.arrange(ex1, ex2, ex3, ex4, ncol=4, widths=c(1,1,1,1), padding = unit(0, "line"), newpage = F)

#### Assemble and save -------------------------------------------------------
mappanel <- grid.arrange(mapLandWater,mapCLCchange,plotAreaChange, ncol=3, widths=c(1,1,1), padding = unit(0, "line"), newpage = F)
fig1 <- grid.arrange(mappanel,expanel, ncol=1, heights=c(2,1), padding = unit(0, "line"), newpage = F)

ggsave(out_path("fig1.png"), fig1, width = 40, height=18, units='cm')
