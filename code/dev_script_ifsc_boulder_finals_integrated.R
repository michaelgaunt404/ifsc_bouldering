

library(janitor)
ifsc_boulder_old


ifsc_boulder_final









gauntlet::make_new_script(folder = "R/code"
                          ,file_name = "script_ifsc_boulder_data_prep.R")




janitor::compare_df_cols(ifsc_boulder_old
                         ,ifsc_boulder_final)


ifsc_boulder_final_raw =
  here("r/data/ifsc_missing_tries_20220730.xlsx") %>%
  readxl::read_xlsx(sheet = "ifsc_missing_tries_20220713") %>%
  select(year,event_name, gender_id, rank, lastname
         ,score, index_og_order, index_route_order
         ,ends_with("_mg")) %>%
  mutate(across(c(starts_with("top_"), starts_with("zone_")), as.numeric))


ifsc_boulder_final_raw$rank %>% unique()
ifsc_boulder_final_raw$year %>% unique()



ifsc_comb = ifsc_boulder_final_raw %>%
  merge(ifsc_boulder_old
        ,by = c('year', 'event_name', 'gender_id', 'rank', 'lastname'
                ,'score', 'index_og_order', 'index_route_order')
        ,all.x = T
        ) %>%
  data.frame() %>%
  janitor::remove_constant() %>%
  select(year, event_id, event_name, gender_id, rank, athlete_id, lastname, country,  rank, score,
         round_name, category_round_id,
         starts_with("Index")
         ,starts_with("top"), starts_with("zone")) %>%
  mutate(across(c(starts_with("zone_"), starts_with("top_")), as.numeric))

ifsc_comb %>%
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
    ,zone_ttl_att = sum(zone_tries_use)
    #same as above
    # ,top_ttl_att_adj = sum(top_tries_adj)
    # ,zone_ttl_att_adj = sum(zone_tries_adj)
  ) %>%
  mutate(top_att_ratio = (1/top_tries_use) %>%
           ifelse(is.infinite(.), 0, .) %>%
           round(2)
         ,top_att_ttl_ratio = (sum(top)/top_ttl_att) %>%
           ifelse(is.infinite(.), 0, .) %>%
           round(2)
         ,zone_att_ratio = (1/zone_tries_use) %>%
           ifelse(is.infinite(.), 0, .) %>%
           round(2)
         ,zone_att_ttl_ratio = (sum(zone)/zone_ttl_att) %>%
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






    ,top_att_ratio = (1/top_tries_use) %>%
      ifelse(is.infinite(.), 0, .) %>%
      round(2)
    ,zone_att_ratio = (1/zone_tries_use) %>%
      ifelse(is.infinite(.), 0, .) %>%
      round(2)
    ,top_att_ratio_adj = (1/top_tries_adj) %>%
      ifelse(is.infinite(.), 0, .) %>%
      round(2)
    ,zone_att_ratio_adj = (1/zone_tries_adj) %>%
      ifelse(is.infinite(.), 0, .) %>%
      round(2)) %>%
  ungroup() %>%
  group_by(event_id, athlete_id, round_name) %>%
  mutate(zone_att_ratio_mean = mean(zone_att_ratio) %>%
           round(2)
         ,top_att_ratio_mean = mean(top_att_ratio) %>%
           round(2)
         ,zone_att_ratio_adj_mean = mean(zone_att_ratio_adj) %>%
           round(2)
         ,top_att_ratio_adj_mean = mean(top_att_ratio_adj) %>%
           round(2)) %>%
  ungroup() %>%
  select(year, event_id, event_name, gender_id, rank, athlete_id, lastname, country,  rank, score,
         round_name, category_round_id,
         starts_with("Index")
         ,starts_with("top"), starts_with("zone"))





