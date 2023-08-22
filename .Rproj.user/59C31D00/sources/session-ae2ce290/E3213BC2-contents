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
ifsc_data_comp_rank = tar_read(ifsc_data_comp_rank) %>%  
  data.frame() %>%  
  mutate(flag_medal = case_when(rank<4~"medal", T~"no_medal")
         ,count = 1)

#country_statistics=============================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


temp = ifsc_data_comp_rank %>%  
  filter(round_flag == 1) %>%  
  count_percent_zscore(
    grp_c = c('year', 'gender_id', 'country', 'flag_medal')
    ,grp_p = c('year', 'gender_id', 'country')
    ,col = count
  ) %>%  
  arrange(year) %>% 
  complete(year, gender_id, country, flag_medal) %>%  
  mutate(across(c(count, percent), ~replace_na(.x, 0))) %>% 
  group_by(gender_id, country, flag_medal) %>%  
  mutate(cumm_count = cumsum(count)) %>%  
  ungroup()

temp_ttl = temp %>%  
  group_by(year, gender_id, country) %>%  
  summarise(count = sum(count)) %>%  
  group_by(gender_id, country) %>%  
  mutate(cumm_count = cumsum(count)) %>%  
  ungroup() %>%  
  arrange(country, desc(year)) %>%  
  filter(country == "USA")
  group_by(year, country)

temp_ttl %>%  
  filter(country == "USA") %>%  
  complete(year, gender_id, country) %>%  
  mutate(count = replace_na(count, 0))

temp_ttl %>%  
  filter(country == "USA") %>%  
  ggplot() +
  geom_line(aes(year, cumm_count, group = country)) + 
  facet_grid(rows = vars(gender_id))

temp %>%  
  filter(country == "USA") %>%
  ggplot() +
  geom_line(aes(year, cumm_count, group = country)) + 
  facet_grid(rows = vars(gender_id)
             ,cols = vars(flag_medal))

temp %>%  
  ggplot() +
  geom_line(aes(year, count, group = country)) + 
  facet_grid(rows = vars(gender_id)
             ,cols = vars(flag_medal))

temp %>%  
  filter(country == "USA") %>% 
  ggplot() +
  geom_line(aes(year, count, group = country)) + 
  facet_grid(rows = vars(gender_id)
             ,cols = vars(flag_medal))






























yolo = ifsc_data_comp_results %>%  
  filter(round_name == "Final") %>% 
  select(year, event_id, gender_id, athlete_id, rank, top_ttl_top, zone_ttl_zone) %>% 
  unique() %>% 
  mutate(count = 1
         ,first_place = case_when(rank == 1~T, T~F)
         ,top_two = case_when(rank <= 2~T, T~F))

yolo %>%  
  count_percent_zscore(grp_c = c('first_place', "top_ttl_top")
                       ,grp_p = c('first_place')
                       ,col = count)

yolo %>%  
  count_percent_zscore(grp_c = c('first_place', "top_ttl_top")
                       ,grp_p = c("top_ttl_top")
                       ,col = count)

#looks like I got the same result as above 
yolo %>% 
  # filter(year > 2017) %>%
  count_percent_zscore(grp_c = c("top_ttl_top", 'top_two')
                       ,grp_p = c("top_ttl_top")
                       ,col = count)



##sub header 1==================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##sub header 2==================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#script end=====================================================================










































