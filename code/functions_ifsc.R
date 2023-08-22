
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
  
  janitor::compare_df_cols(data_scraped, data_missing)
  
  data_scraped %>% 
    select(year,event_name, gender_id, rank, lastname
           ,score, index_og_order, index_route_order
           ,ends_with("_mg"))
  
  data_missing %>% 
    select(year,event_name, gender_id, rank, lastname
           ,score, index_og_order, index_route_order
           ,ends_with("_mg")) %>%
    mutate(across(c(starts_with("top_"), starts_with("zone_")), as.numeric))
  
  #combine data streams
  data_comb = data_missing_pro %>%
    merge(data_scraped
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
  


}


##vary/const cols===============================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



##sub header 1==================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##sub header 2==================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#script end=====================================================================

















