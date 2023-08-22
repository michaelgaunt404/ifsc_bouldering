#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# This is script [[insert brief readme here]]
#
# By: mike gaunt, michael.gaunt@wsp.com
#
# README: [[insert brief readme here]]
#-------- [[insert brief readme here]]
#
# *please use 80 character margins
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#library set-up=================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev
library(tidyverse)
library(gauntlet)
library(here)

#path set-up====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev

#source helpers/utilities=======================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev

#source data====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev
#area to upload data with and to perform initial munging
#please add test data here so that others may use/unit test these scripts

##original data that was scraped in its raw form-----
#---->need to make sure it wasn't altered (it probably was)
ifsc_boulder_old =
  here("data/climbing_20220630.rds") %>% readRDS()

##data with filled missing
# ifsc_boulder_final_raw =
#   read_xlsx_allFiles(data_location  = paste0(here("data/"), "/")
#                      ,specifically = "ifsc_missing_tries"
#                      # ,sheet = "ifsc_missing_tries_20220713"
#                      ) %>%
#   rev() %>%
#   .[[1]] %>%
#   select(year,event_name, gender_id, rank, lastname
#          ,score, index_og_order, index_route_order
#          ,ends_with("_mg")) %>%
#   mutate(across(c(starts_with("top_"), starts_with("zone_")), as.numeric))

ifsc_boulder_final_raw = tar_read("ifsc_data_missing") %>%  
  read_xlsx() %>% 
  select(year,event_name, gender_id, rank, lastname
                  ,score, index_og_order, index_route_order
                  ,ends_with("_mg")) %>%
           mutate(across(c(starts_with("top_"), starts_with("zone_")), as.numeric))

#processing data================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ifsc_comb_data = ifsc_boulder_final_raw %>%
  merge(ifsc_boulder_old
        ,by = c('year', 'event_name', 'gender_id', 'rank', 'lastname'
                ,'score', 'index_og_order', 'index_route_order')
        ,all.x = T
  ) %>%
  data.frame() %>%
  janitor::remove_constant() %>%
  select(year, event_id, event_name, gender_id, rank, athlete_id
         ,lastname, country,  rank, score,round_name, category_round_id
         ,starts_with("Index"),starts_with("top"), starts_with("zone")) %>%
  mutate(across(c(starts_with("zone_"), starts_with("top_")), as.numeric))

ifsc_comb_data_pro = ifsc_comb_data %>%
  mutate(top_tries_use = case_when(top_tries == 0~top_mg, T~top_tries)
         ,zone_tries_use = case_when(zone_tries == 0~zone_mg, T~zone_tries)
         ,top_zone_diff = top_tries_use-zone_tries_use
         ,across(c(top_tries_use, zone_tries_use, top_zone_diff)
                 ,~replace_na(.x, 999))) %>%
  group_by(event_name, athlete_id, category_round_id) %>%
  mutate(
    top_cum = cumsum(top)
    ,zone_cum = cumsum(zone)
    #would like these in but issue with NAs in there
    #TODO fix
    # ,top_tries_cum = cumsum(top_tries_use)
    # ,zone_tries_cum = cumsum(zone_tries_use)
    #hiding these for now since I manually recorded these values
    # ,top_tries_adj = ifelse(top == FALSE, 1000, top_tries_use)
    # ,zone_tries_adj = ifelse(zone == F, 1000, zone_tries_use)
    ,top_ttl_att = sum(top_tries_use)
    ,top_ttl_att_og = sum(top_tries)
    ,zone_ttl_att = sum(zone_tries_use)
    ,zone_ttl_att_og = sum(zone_tries)
    #same as above
    # ,top_ttl_att_adj = sum(top_tries_adj)
    # ,zone_ttl_att_adj = sum(zone_tries_adj)
  ) %>%
  mutate(top_att_ratio = (1/top_tries_use) %>%
           ifelse(is.infinite(.), 0, .) %>%
           round(2)
         ,top_ttl_top = sum(top)
         ,top_att_ttl_ratio = (top_ttl_top/top_ttl_att) %>%
           ifelse(is.infinite(.), 0, .) %>%
           round(2)
         ,zone_att_ratio = (1/zone_tries_use) %>%
           ifelse(is.infinite(.), 0, .) %>%
           round(2)
         ,zone_ttl_zone = sum(zone)
         ,zone_att_ttl_ratio = (zone_ttl_zone/zone_ttl_att) %>%
           ifelse(is.infinite(.), 0, .) %>%
           round(2)
         #same as above
         # ,top_att_ratio_adj = (1/top_tries_adj) %>%
         #   ifelse(is.infinite(.), 0, .) %>%
         #   round(2)
         # ,zone_att_ratio_adj = (1/zone_tries_adj) %>%
         #   ifelse(is.infinite(.), 0, .) %>%
         #   round(2)
  ) %>%
  ungroup() %>%
  select(year, event_id, event_name, gender_id, rank, athlete_id, lastname, country,  rank, score,
         round_name, category_round_id,
         starts_with("Index")
         ,starts_with("top"), starts_with("zone"))

##vary/const cols===============================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#defines which columns vary and which ones do not
#used for column de-/selection

subset_for_constant_cols = ifsc_comb_data_pro %>%
  filter(year == 2019
         ,event_id == 1112
         ,rank == 3
         ,gender_id == 7)

index_cols_varying = subset_for_constant_cols %>%
  janitor::remove_constant() %>%
  colnames()

subset_for_constant_cols %>%
  select(!all_of(index_cols_varying)) %>%
  unique() %>%
  glimpse()

##vary/const cols===============================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ifsc_bouldering_results = ifsc_comb_data_pro %>%
  select(!all_of(index_cols_varying)) %>%
  unique()

ifsc_boulder_events = ifsc_boulder %>%
  select(event_id, event_name, year) %>%
  unique() %>%
  arrange(year, event_id) %>%
  mutate(index_order = dplyr::row_number())

ifsc_boulder_results = ifsc_boulder %>%
  select(index_route_order, year:category_round_id
         ,top_cum, top_ttl_att, top_ttl_att_adj
         ,top_att_ratio_mean, top_att_ratio_adj_mean
         ,zone_cum, zone_ttl_att,zone_ttl_att_adj
         ,zone_att_ratio_mean, zone_att_ratio_adj_mean) %>%
  group_by(event_id, athlete_id, category_round_id) %>%
  filter(index_route_order == max(index_route_order)) %>%
  ungroup()


##sub header 1==================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##sub header 2==================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#script end=====================================================================










































