#### 05 - Figure 4: biome-stratified urban/cropland change intensity panels
#
# Run from the repo root: Rscript R/05_figure4_biome_panels.R
# Depends on: R/00_setup.R (sourced below).
# Writes: <OUTPUTS_DIR>/fig4.png
#
# One row per biome (All, then each RESOLVE biome present in the EEA-39
# grid), left column urban-driven change, right column cropland-driven
# change, arrows show gross loss/gain, bars show net change.
#
# Author: Zander Venter

source("R/00_setup.R")

#### Compute net-change ratios per biome --------------------------------------
biomes <- clc_change %>% drop_na(BIOME_NAME) %>% filter(BIOME_NAME != 'N/A') %>%
  group_by(BIOME_NAME) %>%
  summarise() %>%pull(BIOME_NAME)
biomes <- c("All" ,biomes)

clc_netChange_ratio_all <- tibble()

for (b in biomes){

  if (b == 'All'){
    clc_change_filt <- clc_change
  } else {
    clc_change_filt <- clc_change %>% filter(BIOME_NAME == b)
  }

  clc_nature_tot <- clc_change_filt %>%
    filter(!clc_class %in% c('Marine inlets', 'Rivers and lakes') ) %>%
    filter(!clc_class %in% c('Urban', 'Cropland')) %>%
    # Excluding habitats recovered since 2000 (i.e. they were urban or cropland in 2000)
    filter(!str_detect(clc_class, 'Gain')) %>%
    group_by(landwater) %>%
    summarise(totArea_landwater = sum(area, na.rm=T)/1000000) %>%
    ungroup() %>%
    mutate(percArea_landwater = totArea_landwater/sum(totArea_landwater)*100)

  clc_change_summary <- clc_change_filt %>%
    filter(str_detect(clc_class, 'Loss|Gain')) %>%
    group_by(clc_class, landwater)%>%
    summarise(change_area = sum(area, na.rm=T)/1000000) %>%
    left_join(clc_nature_tot) %>%
    mutate(clc_base = str_split(clc_class, ' ') %>% map_chr(3))%>%
    mutate(clc_lossgain = str_split(clc_class, ' ') %>% map_chr(1))

  clc_netChange_ratio <- clc_change_summary %>%
    ungroup() %>%
    dplyr::select(landwater, clc_lossgain, clc_base, change_area) %>%
    pivot_wider(names_from=clc_lossgain, values_from=change_area) %>%
    # account for NA gain values (e.g. Tundra ecoregion has none)
    mutate(Gain = ifelse(is.na(Gain), 0, Gain)) %>%
    mutate(netLoss = Loss - Gain,
           recoverPerc = Gain / Loss * 100) %>%
    left_join(clc_nature_tot) %>%
    mutate(change_ratio_netloss = netLoss / totArea_landwater * 100,
           change_ratio_loss = Loss / totArea_landwater * 100,
           change_ratio_gain = Gain / totArea_landwater * 100)

  clc_netChange_ratio_all <- clc_netChange_ratio_all %>%
    bind_rows(clc_netChange_ratio %>%
                mutate(biome = b))
}

#### Per-biome panel builder --------------------------------------------------
gridToMap <- grid%>%
  mutate(lon =st_coordinates(st_centroid(geometry))[,1]) %>%
  filter(lon > 2250000)

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
      strip.background = element_blank(),
      strip.text = element_blank()
    )

  if (legend) {
    pLeft <- pLeft+
      # Dummy legend
      geom_line(aes(color = "Gross loss"), alpha = 0) +
      geom_line(aes(color = "Gross gain"), alpha = 0) +
      geom_line(aes(color = "Net change"), alpha = 0) +
      scale_color_manual(values = c("Gross loss" = "black", "Gross gain" = "#6fdbde", "Net change" = "#bb63b1"),
                         breaks = c("Gross loss","Gross gain","Net change")) +
      guides(fill='none',color = guide_legend(override.aes = list(alpha = 1, linewidth = 2))) +
      theme(legend.title= element_blank(),
            legend.background = element_blank(),
            legend.position = c(0.2, 0.5))
  } else {
    pLeft <- pLeft + theme(legend.position = 'none')
  }

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
      strip.background = element_blank(),
      strip.text = element_blank()
    )

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

#### Assemble and save -------------------------------------------------------
# Biomes selected here match the manuscript's Figure 4 panel set - re-check
# `biomes` (printed above) if the underlying ecoregion data changes.
cp1 <- makeChangePlot('All', 'a)', c(-1.2,0.6), FALSE, FALSE)
cp2 <- makeChangePlot('Tundra', 'b)', c(-1.2,0.6), TRUE, FALSE)
cp3 <- makeChangePlot('Boreal Forests/Taiga', 'c)', c(-1.2,0.6), FALSE, FALSE)
cp4 <- makeChangePlot('Temperate Broadleaf & Mixed Forests', 'd)', c(-1.2,0.6),FALSE,  FALSE)
cp5 <- makeChangePlot('Temperate Conifer Forests', 'e)', c(-1.2,0.6),FALSE,  FALSE)
cp6 <- makeChangePlot('Mediterranean Woodlands & Scrub', 'f)', c(-1.2,0.6), FALSE, TRUE)
fig4 <- grid.arrange(cp1, cp2, cp3, cp4, cp5, cp6, ncol=1, heights=c(1,1,1,1,1,1.2), padding = unit(0, "line"), newpage = F)

ggsave(out_path("fig4.png"), fig4, width = 15, height=20, units='cm')
