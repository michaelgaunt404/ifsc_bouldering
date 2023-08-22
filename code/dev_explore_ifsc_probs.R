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
ifsc_data_comb_pro = tar_read("ifsc_data_comb_pro") %>%  
  data.frame()

ifsc_reshaped = ifsc_data_comb_pro %>%  
  filter(round_name == "Final") %>% 
  select(event_id, gender_id,athlete_id, index_og_order, top, rank) %>%  
  # mutate(top = case_when(top == T~1, T~0) %>%  as.character()) %>% 
  pivot_wider(names_from = index_og_order
              ,values_from = top
              ,names_glue = "boulder_{index_og_order}") %>%  
  mutate(count = 1
         ,first_place = case_when(rank == 1~T, T~F)
         ,top_two = case_when(rank <= 2~T, T~F)) %>%  
  mutate(boulder_1_2 = boulder_1 & boulder_2
         ,boulder_1_2_3 = (boulder_1 & boulder_2) & boulder_3
         ,boulder_1_2_3_4 = ((boulder_1 & boulder_2) & boulder_3 & boulder_4))


ifsc_reshaped %>% filter(boulder_1_2_3) %>%  summarise(won_percent = mean(top_two))

ifsc_reshaped %>%  
  count_percent_zscore(grp_c = c('boulder_1', 'first_place')
                       ,grp_p = c('boulder_1')
                       ,col = count)

ifsc_reshaped %>%  
  count_percent_zscore(grp_c = c('first_place', 'boulder_1')
                       ,grp_p = c('first_place')
                       ,col = count)


ifsc_reshaped %>%  
  count_percent_zscore(grp_c = c('boulder_1_2', 'first_place')
                       ,grp_p = c('boulder_1_2')
                       ,col = count)

ifsc_reshaped %>%  
  count_percent_zscore(grp_c = c('boulder_1_2_3_4', 'first_place')
                       ,grp_p = c('boulder_1_2_3_4')
                       ,col = count)

ifsc_reshaped %>%  
  count_percent_zscore(grp_c = c('boulder_1_2_3_4', 'first_place')
                       ,grp_p = c('boulder_1_2_3_4')
                       ,col = count)

#main header====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ifsc_data_comp_results = tar_read("ifsc_data_comp_results") %>%  
  data.frame()

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










































