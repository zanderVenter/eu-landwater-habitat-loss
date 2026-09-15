#### 06 - Figure 5: pre/post-loss and pre/post-recovery CLC level-3 drivers
#
# Run from the repo root: Rscript R/06_figure5_l3_drivers.R
# Depends on: R/00_setup.R (sourced below).
# Writes: <OUTPUTS_DIR>/fig5.png
#
# Panels: (a) pre-loss land cover, (b) pre-recovery land cover,
# (c) post-loss land cover, (d) post-recovery land cover - each showing the
# percentage share of CLC level-3 classes involved, split by landwater zone.
#
# Author: Zander Venter

source("R/00_setup.R")

makeChangeDriverPlot <- function(){

  #### Panels a, b: pre-loss / pre-recovery (2000) land cover -----------------
  clc_change_l3_perc_00 <- clc_change_l3%>%
    filter(change_type == 'changed_from_2000') %>%
    group_by(landwater, clc_lossgain, clc_3) %>%
    summarise(area = sum(area)/1000000) %>%
    group_by(landwater, clc_lossgain) %>%
    mutate(areaTot = sum(area),
           areaPerc = area/areaTot*100) %>%
    filter(areaPerc > 1) %>%
    mutate(clc_3 = substr(clc_3, 1, 42)) %>%
    group_by(clc_3, clc_lossgain) %>%
    mutate(ordering = sum(areaPerc))

  cd1 <- clc_change_l3_perc_00    %>%
    filter(clc_lossgain == 'Loss')%>%
    ggplot(aes(x=reorder(clc_3, ordering), y = areaPerc, fill=landwater)) +
    geom_col(alpha=0.8, position = position_dodge(width=0.75,preserve = "single"), width=0.75, color='white') +
    coord_flip() +
    scale_x_discrete(labels = scales::label_wrap(width = 25)) +
    scale_fill_manual(values = c('#eadc91', '#62c8ca', '#4582bb')) +
    scale_color_manual(values = c('#eadc91',  '#62c8ca','#4582bb')) +
    labs(x = 'CLC level 3 land use category',
         y = 'Area percentage',
         title = 'a) Pre-loss land cover') +
    theme(legend.position = 'none',
          axis.title.y = element_blank(),
          axis.text.y = element_text(size=8),
          legend.title = element_blank(),
          legend.background = element_blank())

  cd2 <- clc_change_l3_perc_00    %>%
    filter(clc_lossgain == 'Gain')%>%
    ggplot(aes(x=reorder(clc_3, ordering), y = areaPerc, fill=landwater)) +
    geom_col(alpha=0.8, position = position_dodge(width=0.75,preserve = "single"), width=0.75, color='white') +
    coord_flip() +
    scale_x_discrete(labels = scales::label_wrap(width = 25)) +
    scale_fill_manual(values = c('#eadc91', '#62c8ca', '#4582bb')) +
    scale_color_manual(values = c('#eadc91',  '#62c8ca','#4582bb')) +
    labs(x = 'CLC level 3 land use category',
         y = 'Area percentage',
         title = 'b) Pre-recovery land cover') +
    theme(legend.position = 'none',
          axis.title.y = element_blank(),
          axis.text.y = element_text(size=8),
          legend.title = element_blank(),
          legend.background = element_blank())

  #### Panels c, d: post-loss / post-recovery (2018) land cover ----------------
  clc_change_l3_perc <- clc_change_l3%>%
    filter(change_type == 'changed_to_2018') %>%
    group_by(landwater, clc_lossgain, clc_3) %>%
    summarise(area = sum(area)/1000000) %>%
    group_by(landwater, clc_lossgain) %>%
    mutate(areaTot = sum(area),
           areaPerc = area/areaTot*100) %>%
    filter(areaPerc > 1) %>%
    mutate(clc_3 = substr(clc_3, 1, 42)) %>%
    group_by(clc_lossgain, clc_3) %>%
    mutate(ordering = sum(areaPerc))

  cd3 <- clc_change_l3_perc %>%
    filter(clc_lossgain == 'Loss')   %>%
    ggplot(aes(x=reorder(clc_3, ordering), y = areaPerc, fill=landwater)) +
    geom_col(alpha=0.8, position = position_dodge(width=0.75,preserve = "single"), width=0.75, color='white') +
    coord_flip() +
    scale_x_discrete(labels = scales::label_wrap(width = 25)) +
    scale_fill_manual(values = c('#eadc91', '#62c8ca', '#4582bb')) +
    scale_color_manual(values = c('#eadc91',  '#62c8ca','#4582bb')) +
    labs(x = 'CLC level 3 land use category',
         y = 'Area percentage',
         title = 'c) Post-loss land cover') +
    theme(legend.position = c(0.7, 0.3),
          axis.title.y = element_blank(),
          axis.text.y = element_text(size=8),
          legend.title = element_blank(),
          legend.background = element_blank())

  cd4 <- clc_change_l3_perc%>%
    filter(clc_lossgain == 'Gain')   %>%
    ggplot(aes(x=reorder(clc_3, ordering), y = areaPerc, fill=landwater)) +
    geom_col(alpha=0.8, position = position_dodge(width=0.75,preserve = "single"), width=0.75, color='white') +
    coord_flip() +
    scale_x_discrete(labels = scales::label_wrap(width = 25)) +
    scale_fill_manual(values = c('#eadc91', '#62c8ca', '#4582bb')) +
    scale_color_manual(values = c('#eadc91',  '#62c8ca','#4582bb')) +
    labs(x = 'CLC level 3 land use category',
         y = 'Area percentage',
         title = 'd) Post-recovery land cover') +
    theme(legend.position = 'none',
          axis.title.y = element_blank(),
          axis.text.y = element_text(size=8),
          legend.title = element_blank(),
          legend.background = element_blank())

  fig5 <- grid.arrange( cd1, cd2, cd3, cd4, ncol=2, widths=c(1,1), padding = unit(1, "lines"), newpage = F)

  return (fig5)
}

ggsave(out_path("fig5.png"), makeChangeDriverPlot(), width = 22, height=22, units='cm')
