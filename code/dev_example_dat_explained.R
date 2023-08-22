

data_missing = tar_read("ifsc_data_missing") %>%  
  readxl::read_xlsx()

data_missing %>%  
  filter(!is.na(top_mg))


data = tar_read("ifsc_data_comb_pro")




#this is an example of the data 
#2021 is an example of event where IFSC correctly labels attempts
#2019 is an example of event where I perfrom the correct attempts labeling
#--observation:
#----notice top_MG has NAs but but missing data has them filled out 
#----the older event has 0s for tries when attempt is false
data %>%  
  filter((year == 2021 & gender_id == 3 & rank == 3 & event_id == 1187) |
           (year == 2019 & gender_id == 3 & rank == 4 & event_id == 1112)) %>%  
  View()











