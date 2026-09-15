#### 02 - Table 1 / Table S2: consumption-formation-net-change accounting
#
# Run from the repo root: Rscript R/02_table1_accounting.R
# Depends on: R/00_setup.R (sourced below).
# Writes: <OUTPUTS_DIR>/change_account_eu.csv (Table 1),
#         <OUTPUTS_DIR>/change_account_countries.csv (Table S2, feeds
#         Figure 2c/d via R/03_figure2_country_maps.R).
#
# Builds an ecosystem-accounting-style opening/consumption/formation/
# net-change/closing table for nature, split by urban vs. cropland flow and
# by inland/coastal/riparian zone - first aggregated across the whole EEA-39
# (change_account_eu, Table 1), then per country (change_account_country,
# Table S2).
#
# Author: Zander Venter

source("R/00_setup.R")

# Get nature transition account from clc_change_country
clc_change_country_trans <- clc_change_country %>%
  filter(str_detect(clc_class, 'Loss|Gain')) %>%
  mutate(transition = ifelse(str_detect(clc_class, 'Loss'), 'consumption', 'formation'),
         lc_flow = str_split(clc_class, ' ') %>% map_chr(3)) %>%
  dplyr::select(country, transition, lc_flow, landwater, area)

clc_change_country_trans_netChng <- clc_change_country_trans %>%
  pivot_wider(values_from = area, names_from = c( 'transition'))  %>%
  mutate(across(everything(), ~replace_na(., 0)))%>%
  mutate(netChange = formation - consumption)

clc_end_simp <- clc_change_country_trans_netChng %>%
  group_by(country, landwater) %>%
  summarise(netChange = sum(netChange)) %>%
  left_join(clc_base_simp%>%
              filter(lc == 'nature') ) %>%
  mutate(areaEnd = areaBase + netChange)

#### Table 1: EU-39 accounting table -----------------------------------------
change_account_eu <- clc_change_country_trans %>%
  # Add in net change areas
  bind_rows(clc_change_country_trans_netChng %>%
              mutate(transition = 'net_change') %>%
              dplyr::select(country, lc_flow, landwater, transition, area=netChange)) %>%
  # Add in opening areas
  bind_rows(clc_base_simp %>%
              filter(lc == 'nature') %>%
              mutate(lc_flow = 'urban') %>%
              mutate(transition = 'opening') %>%
              dplyr::select(country, lc_flow, landwater, transition, area=areaBase)) %>%
  # Add in closing areas
  bind_rows(clc_end_simp %>%
              filter(lc == 'nature') %>%
              mutate(lc_flow = 'urban') %>%
              mutate(transition = 'closing') %>%
              dplyr::select(country, lc_flow, landwater, transition, area=areaEnd)) %>%
  left_join(clc_base_simp%>%
              filter(lc == 'nature') %>%
              dplyr::select(-lc)) %>%

  group_by(lc_flow, transition, landwater) %>%
  summarise(area = sum(area),
            areaBase= sum(areaBase)) %>%

  group_by( transition) %>%
  mutate(areaTransition = sum(area),
         area_yr = area/18,
         area_perc = area / areaBase *100,
         area_perc_trans = area / areaTransition * 100) %>%
  gather(metric, val, area, area_yr, area_perc, area_perc_trans) %>%
  dplyr::select(-areaBase, -areaTransition) %>%
  pivot_wider(values_from = val, names_from = c('landwater', 'lc_flow')) %>%
  mutate(across(everything(), ~replace_na(., 0))) %>%
  # filter out other area metrics for opening and closing stocks
  filter(! (str_detect(transition, 'opening|closing') & str_detect(metric, '_yr|_perc')))%>%
  # filter out area_perc_trans for net change
  filter(!(transition == 'net_change' & metric == 'area_perc_trans')) %>%
  mutate(transition = factor(transition, levels = c('opening', 'consumption', 'formation', 'net_change', 'closing'))) %>%
  arrange(transition)

change_account_eu %>%
  write_csv(out_path('change_account_eu.csv'))

#### Table S2: per-country accounting table -----------------------------------
change_account_country <- clc_change_country_trans %>%
  # Add in net change areas
  bind_rows(clc_change_country_trans_netChng %>%
              mutate(transition = 'net_change') %>%
              dplyr::select(country, lc_flow, landwater, transition, area=netChange)) %>%
  # Add in opening areas
  bind_rows(clc_base_simp %>%
              filter(lc == 'nature') %>%
              mutate(lc_flow = 'urban') %>%
              mutate(transition = 'opening') %>%
              dplyr::select(country, lc_flow, landwater, transition, area=areaBase)) %>%
  # Add in closing areas
  bind_rows(clc_end_simp %>%
              filter(lc == 'nature') %>%
              mutate(lc_flow = 'urban') %>%
              mutate(transition = 'closing') %>%
              dplyr::select(country, lc_flow, landwater, transition, area=areaEnd)) %>%
  left_join(clc_base_simp%>%
              filter(lc == 'nature') %>%
              dplyr::select(-lc)) %>%
  group_by(country, transition) %>%
  mutate(areaTransition = sum(area),
         area_yr = area/18,
         area_perc = area / areaBase *100,
         area_perc_trans = area / areaTransition * 100) %>%
  gather(metric, val, area, area_yr, area_perc, area_perc_trans) %>%
  dplyr::select(-areaBase, -areaTransition) %>%
  pivot_wider(values_from = val, names_from = c('lc_flow', 'landwater')) %>%
  mutate(across(everything(), ~replace_na(., 0))) %>%
  # filter out other area metrics for opening and closing stocks
  filter(! (str_detect(transition, 'opening|closing') & str_detect(metric, '_yr|_perc')))%>%
  # filter out area_perc_trans for net change
  filter(!(transition == 'net_change' & metric == 'area_perc_trans')) %>%
  mutate(transition = factor(transition, levels = c('opening', 'consumption', 'formation', 'net_change', 'closing'))) %>%
  arrange(transition)

change_account_country_to_export <- change_account_country %>%
  arrange(country) %>%
  filter(transition == 'net_change') %>%
  filter(metric %in% c('area_yr', 'area_perc'))

change_account_country_to_export %>%
  write_csv(out_path('change_account_countries.csv'))

#### Optional: quick-look heatmap table (not part of the manuscript figures) --
# A gt/gtExtras colour-coded table of net change percentage by country and
# landwater/lc_flow - handy for spot-checking outliers, not saved to disk.
if (interactive()) {
  library(gt)
  library(gtExtras)
  table_data <- clc_change_country_trans_netChng %>%
    left_join(clc_base_simp %>%
                filter(lc == 'nature') %>%
                dplyr::select(-lc)) %>%
    mutate(netChange_perc = netChange / areaBase * 100) %>%
    select(country, landwater, lc_flow, netChange_perc) %>%
    pivot_wider(names_from = c('landwater', 'lc_flow'), values_from = netChange_perc)

  table_data %>%
    gt() %>%
    gt_color_rows(
      columns = -country,
      palette = c( "red", "orange", "yellow"),
      domain =  NULL
    ) %>%
    tab_header(
      title = "Net Change Percentage by Country",
      subtitle = "Color intensity represents magnitude of change"
    ) %>%
    fmt_number(columns = -country, decimals = 2) %>%
    gt_theme_538()
}
