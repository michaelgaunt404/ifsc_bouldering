#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# This is is a scratch file for development use and exploration.
#
# By: mike gaunt, michael.gaunt@wsp.com
#
# README: general scratch file
#-------- [[insert brief readme here]]
#
# *please use 80 character margins
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#library set-up=================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev
library(tidyverse)
library(here)
library(data.table)
library(lubridate)
library(roll)
library(plotly)
library(crosstalk)
library(janitor)
library(dplyr)
library(gauntlet)

#path set-up====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev

#source helpers/utilities=======================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev
source(here::here("R/code/helpers_general.r"))
source(here::here("R/code/helpers_plotly.r"))
source(here::here("R/code/helpers_DT.r"))

#source data====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev
#area to upload data with and to perform initial munging
#please add test data here so that others may use/unit test these scripts
ifsc_boulder_old =
  here("R/data/climbing_20220630.rds") %>% readRDS()  %>%
  # here("r/data/ifsc_missing_tries_20220716.xlsx") %>%
  # readxl::read_xlsx(sheet = "ifsc_missing_tries_20220713") %>%
  arrange(desc(year), event_name, category_round_id, rank, index_route_order)  %>%
  group_by(event_name, athlete_id, category_round_id) %>%
  mutate(
    top_cum = cumsum(top)
    ,zone_cum = cumsum(zone)
    ,top_tries_adj = ifelse(top == FALSE, 1000, top_tries)
    ,zone_tries_adj = ifelse(zone == F, 1000, zone_tries)
    ,top_ttl_att = sum(top_tries)
    ,zone_ttl_att = sum(zone_tries)
    ,top_ttl_att_adj = sum(top_tries_adj)
    ,zone_ttl_att_adj = sum(zone_tries_adj)
    ,top_att_ratio = (1/top_tries) %>%
      ifelse(is.infinite(.), 0, .) %>%
      round(2)
    ,zone_att_ratio = (1/zone_tries) %>%
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






  ifsc_boulder_events = ifsc_boulder %>%
    select(event_id, event_name, year) %>%
    unique() %>%
    arrange(year, event_id) %>%
    mutate(index_order = dplyr::row_number())

  ifsc_boulder_results = ifsc_boulder %>%
    select(index_route_order, year:category_round_id
           ,top_cum, top_ttl_att, top_ttl_att_adj, top_att_ratio_mean, top_att_ratio_adj_mean
           ,zone_cum, zone_ttl_att,zone_ttl_att_adj, zone_att_ratio_mean, zone_att_ratio_adj_mean) %>%
    group_by(event_id, athlete_id, category_round_id) %>%
    filter(index_route_order == max(index_route_order)) %>%
    ungroup()


  #exploration: QC================================================================
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #notes:
  #---number of tops/zones and respective attempts are okay
  #---data does not include failed attempts - so total boulder attempts
  #-------or if you didn't top or zone

  ifsc_boulder %>%
    filter(round_name == "Final"
           ,year == 2019
           ,str_detect(event_name,"Munich")
           # ,gender_id ==
           # ,lastname %in% c("Ogata", "Hoyer")
    )

  ifsc_boulder %>%
    filter(#round_name == "Final",
      top == F & top_tries > 0) %>%
    mutate(top_tries )
  pull(year) %>%
    unique() %>%
    sort()


  #exploration: general rankings==================================================
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ifsc_boulder_results_small = ifsc_boulder_results %>%
    select(event_id, athlete_id, lastname, rank, country, gender_id, year) %>%
    unique()

  temp_plot_athlete_avg_rank = ifsc_boulder_results_small %>%
    group_by(athlete_id, lastname, gender_id, year) %>%
    summarise(rank_mean = mean(rank), .groups = "drop") %>%
    filter(year != 2020) %>%
    ggplot() +
    geom_line(aes(year, rank_mean, group = paste0(lastname, "-", athlete_id))) +
    facet_grid(rows = vars(gender_id), scales = "free") +
    ylim(c(0, NA)) +
    theme(legend.position = "none")

  temp_plot_athlete_avg_rank %>%
    ggplotly()

  temp_plot_team_avg_rank = ifsc_boulder_results_small %>%
    group_by(country, gender_id, year) %>%
    summarise(rank_mean = mean(rank,  na.rm = T), .groups = "drop") %>%
    filter(year != 2020) %>%
    ggplot() +
    geom_line(aes(year, rank_mean,  color = country )) +
    facet_grid(rows = vars(gender_id), scales = "free") +
    ylim(c(0, NA)) +
    theme(legend.position = "none")

  temp_plot_team_avg_rank %>%
    ggplotly()

  #exploration: attempts==========================================================
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  #the reason this looks like this is because of the data change
  #tracking differences in total attemtps

  ifsc_boulder_results %>%
    filter(round_name == "Final") %>%
    group_by(gender_id, year, rank) %>%
    summarise(top_att_mean = mean(top_ttl_att, na.rm = T)
              ,zone_att_mean = mean(zone_ttl_att, na.rm = T),
              .groups = "drop") %>%
    # pivot_longer(cols = )
    ggplot() +
    geom_line(aes(year, top_att_mean, color = as.factor(rank))) +
    facet_grid(rows = vars(gender_id), scales = "free") +
    ylim(c(0, NA)) +
    theme(legend.position = "none")




  #exploration: efficiency======================================================

  ifsc_boulder_results %>%
    filter(round_name == "Final") %>%
    arrange(desc(top_att_ratio_mean-top_att_ratio_adj_mean))

  ###efficiency score skrinkage----
  #difference in DAQ and shift in mean values
  ifsc_boulder_results %>%
    mutate(flag_2019 = year > 2019) %>%
    filter(round_name == "Final") %>%
    ggplot(aes(top_att_ratio_mean, top_att_ratio_adj_mean, color = as.factor(rank))) +
    geom_jitter() +
    facet_grid(cols = vars(flag_2019))

  #yearly trends in efficiency----

  #boxplot
  ifsc_boulder_results %>%
    filter(round_name == "Final") %>%
    ggplot(aes(as.factor(rank), top_att_ratio_mean, color = as.factor(year))) +
    geom_boxplot()

  #median efficiency value per rank per year
  ifsc_boulder_results %>%
    filter(round_name == "Final") %>%
    group_by(year, rank) %>%
    summarise(median = median(top_att_ratio_adj_mean, na.rm = T), .groups = "drop") %>%
    ggplot(aes(rank, median, color = as.factor(year))) +
    geom_line() +
    ylim(c(0, 1))

  ifsc_boulder_results %>%
    filter(round_name == "Final", year > 2019) %>%
    group_by(year, rank) %>%
    summarise(median = median(top_att_ratio_mean, median(top_att_ratio_mean, na.rm = T)), .groups = "drop") %>%
    ggplot(aes(rank, median, color = as.factor(year))) +
    geom_line() +
    ylim(c(0, 1))


  temp = ifsc_boulder_results %>%
    filter(round_name == "Final")  %>%
    group_by(event_id, gender_id) %>%
    mutate(flag_rank_att_top = top_ttl_att < lead(top_ttl_att)
           ,flag_rank_top_att_ratio = zone_att_ratio_adj_mean > lead(zone_att_ratio_adj_mean)) %>%
    ungroup() %>%
    na.omit()


  temp_plot_ranking_eff =  temp %>%
    filter(round_name == "Final") %>%
    group_by(year, gender_id, rank) %>%
    summarise(across(c(flag_rank_att_top, flag_rank_top_att_ratio), mean), .groups = "drop") %>%
    ggplot(aes(rank, flag_rank_top_att_ratio, color = as.factor(year))) +
    geom_line() +
    facet_grid(cols = vars(gender_id))

  temp_plot_ranking_eff %>%
    ggplotly()




  #exploration: country===========================================================


  temp_plot_country_finals_count = ifsc_boulder_results %>%
    mutate(count = 1) %>%
    filter(round_name == "Final") %>%
    count_percent_zscore(grp_c = c(year, gender_id, country),
                         grp_p = c(year, gender_id)) %>%
    group_by(gender_id, country) %>%
    mutate(count = cumsum(count),
           year_crrct = crrct0((year))) %>%
    ggplot(aes(year_crrct, count, color = as.factor(country),  alpha = as.factor(gender_id))) +
    geom_line() +
    scale_alpha_discrete(range = c(.4, .9))
  facet_grid(cols = vars(gender_id))

  temp_plot_country_finals_count %>%
    ggplotly()





  ifsc_boulder %>%
    filter(year == 2021 |year ==  2019
           ,rank == 1
           ,top == F) %>%
    sample_n(10) %>%
    view()


  ifsc_boulder %>%
    filter(round_name == "Final") %>%
    # filter(year == 2021 |year ==  2019
    #        ,rank == 1
    #        # ,top == F) %>%
    select(year, event_name, gender_id, rank, lastname, score, contains("index"), contains("top"), contains("zone")) %>%
    arrange(desc(year), event_name, gender_id, rank) %>%
    write.csv(here("r/data/ifsc_missing_tries.csv"))














#script end=====================================================================

