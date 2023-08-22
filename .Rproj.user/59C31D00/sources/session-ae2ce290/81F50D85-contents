
#source data====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev
#area to upload data with and to perform initial munging
#please add test data here so that others may use/unit test these scripts

##original data that was scraped in its raw form-----
#---->need to make sure it wasn't altered (it probably was)


#processing data================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


##vary/const cols===============================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#defines which columns vary and which ones do not
#used for column de-/selection


ifsc_combine_data = function(data_scraped, data_missing){
  # data_scraped = tar_read("scraped_climbing")
  # data_missing = tar_read("ifsc_data_missing")
  
  #load data
  data_scraped = read_rds(data_scraped)
  data_missing = read_xlsx(data_missing)
  
  #process 
  #data_missing has a very large number of attributes 
  #--many are hold overs from the scraped data after some inital processing - see zone_att_ratio_adj 
  #---these were made by hand but will be REMADE with the new complete data from data_missing
  #--the only data that really needs to be grabbed from data_missing are the *_mg columns
  data_missing_pro = data_missing %>% 
    select(year,event_name, gender_id, rank, lastname
           ,score, index_og_order, index_route_order
           ,ends_with("_mg")) %>%
    mutate(across(c(starts_with("top_"), starts_with("zone_")), as.numeric))
  
  #combine data streams
  data_comb = data_missing_pro %>%
    merge(data_scraped
          ,by = c('year', 'event_name', 'gender_id', 'rank', 'lastname'
                  ,'score', 'index_og_order', 'index_route_order')
          ,all = T
    ) %>%
    data.frame() %>%
    # janitor::remove_constant() %>%
    select(year, event_id, event_name, gender_id, rank, athlete_id
           ,lastname, country,  rank, score,round_name, category_round_id
           ,starts_with("Index"),starts_with("top"), starts_with("zone")) %>%
    mutate(across(c(starts_with("zone_"), starts_with("top_")), as.numeric))
  


}

ifsc_process_combined_data = function(data){
  # data = tar_read("ifsc_data_comb")
  
  #processing steps~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  #step_1~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #reset %_tries attributes between missing and old data
  #missing data has NAs still
  data_pro_1 = data %>%
    mutate(top_tries_use = case_when(top_tries == 0~top_mg, T~top_tries)
           ,zone_tries_use = case_when(zone_tries == 0~zone_mg, T~zone_tries)
           ,top_zone_diff = top_tries_use-zone_tries_use
           ,across(c(top_tries_use, zone_tries_use, top_zone_diff)
                   ,~replace_na(.x, 999)) #dont know why I did this - I think a hold over from the inital procssing
           ) 
  
  #step_2~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #make cum_sum attributes and sum attempts by top/zone and old/new data
  data_pro_2 = data_pro_1 %>%
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
    )
  
  #step_3~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #makes ratio attributes for different top/zone attributes
  data_pro_3 = data_pro_2 %>%
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
    ) 
  
  #step_4~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  data_pro_4 = data_pro_3 %>%
    ungroup() %>%
    select(year, event_id, event_name, gender_id, rank, athlete_id, lastname, country,  rank, score,
           round_name, category_round_id,
           starts_with("Index")
           ,starts_with("top"), starts_with("zone"))
  
  data_pro_4
}

ifsc_comp_results = function(data){
  # data = tar_read("ifsc_data_comb_pro")
  
  ifsc_bouldering_results = data %>%
    select(
      year, event_id, event_name, gender_id, rank, athlete_id
      ,lastname, country, score, round_name, category_round_id
      ,contains("ttl")
      
    ) %>%
    unique()
  
  # ifsc_bouldering_results %>% 
  #   filter(year == 2019
  #          ,event_id == 1112
  #          ,rank == 3
  #          ,gender_id == 7) \
  
  ifsc_bouldering_results
}

ifsc_comp_results_rank = function(data){
  # data = tar_read("ifsc_data_comp_results")
  
  data %>%  
    mutate(round_flag = case_when(round_name == "Final"~1
                                  ,round_name == "Semi-Final"~2
                                  ,T~3)) %>%  
    arrange(year, event_id, gender_id, rank, round_flag) %>%  
    group_by(year, event_id, athlete_id) %>%  
    slice_head(n = 1) %>%
    ungroup() %>%  
    arrange(year, event_id, gender_id, rank)
}

ifsc_boulder_events = function(data){
  # data = tar_read("ifsc_data_comb_pro")
  
  data %>%
    select(event_id, event_name, year) %>%
    unique() %>%
    arrange(year, event_id) %>%
    mutate(index_order = dplyr::row_number())
}


##vary/const cols===============================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



##sub header 1==================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##sub header 2==================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#script end=====================================================================

















