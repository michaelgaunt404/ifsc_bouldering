#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# This is script loads packages used for dev of target objects.
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
library(gauntlet)

pkgs = c("tibble", "tidyverse","here", "readxl", "targets")

package_load(pkgs)

#import data====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#manual step to send/receive data from Google drive location



ifsc_data_comb = targets::tar_read(ifsc_data_comb)
#ifsc_data_comb_pro round/boulder outcomes 
#finals only have corrected try attempts
ifsc_data_comb_pro = targets::tar_read(ifsc_data_comb_pro)
ifsc_data_comp_results = targets::tar_read(ifsc_data_comp_results)
ifsc_data_comp_rank = targets::tar_read(ifsc_data_comp_rank)
ifsc_data_boulder_events = targets::tar_read(ifsc_data_boulder_events)

#helpful targets functions======================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#manual step to send/receive data from Google drive location


#script end=====================================================================





