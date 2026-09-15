#### 07 - Figure S2: bivariate loss/recovery/net-change grid maps
#
# Run from the repo root: Rscript R/07_figureS2_bivariate_maps.R
# Depends on: R/00_setup.R (sourced below).
# Writes: <OUTPUTS_DIR>/bivariateMapFig.png
#
# Six bivariate choropleth maps (2x3: urban loss/gain/net-change, cropland
# loss/gain/net-change) where each grid cell's colour jointly encodes its
# land-water-zone change intensity (x axis) and its inland change intensity
# (y axis), so hotspots that are unusual for land-water vs. inland areas
# stand out. See:
#   https://www.joshuastevens.net/cartography/make-a-bivariate-choropleth-map/
#   https://bluegreenlabs.org/post/map-building-3/
#   https://jakubnowosad.com/posts/2020-08-25-cbc-bp2/
#
# Author: Zander Venter

source("R/00_setup.R")

#### Grid-level change ratio split into inland vs. water (land-water combined) -
clc_nature_grid_tot <- clc_change %>%
  mutate(landwater = ifelse(landwater == 'Inland', 'inland', 'water')) %>%
  filter(!clc_class %in% c('Marine inlets', 'Rivers and lakes') ) %>%
  filter(!clc_class %in% c('Urban', 'Cropland')) %>%
  # Excluding habitats recovered since 2000 (i.e. they were urban or cropland in 2000)
  filter(!str_detect(clc_class, 'Gain')) %>%
  group_by(landwater, id) %>%
  summarise(totArea_landwater = sum(area, na.rm=T)/1000000) %>%
  ungroup() %>%
  mutate(percArea_landwater = totArea_landwater/sum(totArea_landwater)*100)

clc_change_grid_ratio <- clc_change %>%
  mutate(landwater = ifelse(landwater == 'Inland', 'inland', 'water')) %>%
  filter(str_detect(clc_class, 'Loss|Gain')) %>%
  group_by(clc_class, landwater, id)%>%
  summarise(change_area = sum(area, na.rm=T)/1000000) %>%
  left_join(clc_nature_grid_tot, by = c('id', 'landwater')) %>%
  mutate(change_ratio = change_area / totArea_landwater * 100)%>%
  mutate(clc_base = str_split(clc_class, ' ') %>% map_chr(3))%>%
  mutate(clc_lossgain = str_split(clc_class, ' ') %>% map_chr(1))

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

#### Bivariate colour ramps ----------------------------------------------------
bivariate1 <- c('#3b4799', '#8a61af', '#bb63b1',
                '#5a97bc', '#a5acd7', '#ddb0d9',
                '#63c8c9', '#afe4e4', '#e8e8e8')
bivariate3 <- c("#e8e8e8", "#e4d9ac", "#c8b35a",
                "#cbb8d7", "#c8ada0","#af8e53",
                "#9972af", "#976b82", "#804d36")
bivariate4 <- c("#e8e8e8", "#b8d6be", "#73ae80",
                "#b5c0da", "#90b2b3", "#5a9178",
                "#6c83b5", "#567994", "#2a5a5b")
bivariate5 <- c("#804d36","#e8e8e8",'#73ae80',
                "#e8e8e8","#e8e8e8","#e8e8e8",
                "#6c83b5", "#e8e8e8","#2a5a5b")

#### Bivariate map builder -----------------------------------------------------
makeBiVarMap <- function(responseVar, landuse,  logTrans, center, percMin, percMax, title, subtitle, bivPal){
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

  datBiVarToPlot <- df %>%
    mutate(x =df[[varX]],
           y = df[[varY]]) %>%
    dplyr::select(id, x, y)

  limsY <- quantile(datBiVarToPlot$y, probs=c(percMin, percMax))

  if (center){
    interRange <- (limsY[2] - limsY[1])/2
    limsY <- c(-interRange, interRange)
  }

  breaksY <- seq(limsY[1], limsY[2], (limsY[2]-limsY[1])/6)
  labsY <- round(c(breaksY[2], breaksY[4], breaksY[6]), 2)

  if (logTrans){
    labsY <- round(exp(c(breaksY[2], breaksY[4], breaksY[6])), 2)
  }
  legBreaksY <- c(breaksY[2], breaksY[4], breaksY[6])

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
    theme(panel.background = element_blank(),
          panel.border = element_rect(fill=NA))

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

  grob <- ggplotGrob(g1)
  outPlot <- m1 + annotation_custom(grob = grob,
                                    xmin=7370000*0.75,
                                    xmax=7370000*0.95,
                                    ymin=5457000*0.7,
                                    ymax=5457000 *0.95)

  return (outPlot)
}

#### Build the six panels and assemble ----------------------------------------
bv1 <-makeBiVarMap('change_ratio_loss', 'urban', TRUE, FALSE,0.1, 1, 'a) Habitat loss urban expansion', 'nature -> urban', rev(bivariate3))
bv2 <-makeBiVarMap('change_ratio_gain', 'urban', TRUE,FALSE,0.6, 0.99, 'b) Habitat gain urban abandonment', 'urban -> nature', rev(bivariate4))
bv3 <-makeBiVarMap('change_ratio_netchange', 'urban', FALSE,TRUE, 0.1, 0.9,'c) Habitat net change (urban)',  'nature <-> urban', rev(bivariate5))

bv4 <-makeBiVarMap('change_ratio_loss', 'cropland',TRUE,FALSE, 0.3, 0.99, 'd) Habitat loss cropland expansion', 'nature -> cropland', rev(bivariate3))
bv5 <-makeBiVarMap('change_ratio_gain', 'cropland', TRUE,FALSE, 0.3, 1,'e) Habitat gain cropland abandonment',  'cropland -> nature', rev(bivariate4))
bv6 <-makeBiVarMap('change_ratio_netchange', 'cropland',FALSE,TRUE, 0.1, 0.9,'f) Habitat net change (cropland)',  'nature <-> cropland', rev(bivariate5))

bivariateMapFig <- grid.arrange(bv1,bv2,bv3,bv4, bv5, bv6,  ncol=3, heights=c(1,1), padding = unit(0, "line"), newpage = F)

ggsave(out_path("bivariateMapFig.png"), bivariateMapFig, width = 40, height=25, units='cm')
