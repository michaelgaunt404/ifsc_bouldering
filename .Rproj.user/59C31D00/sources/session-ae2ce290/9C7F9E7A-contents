# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed. # nolint

# Set target options:
tar_option_set(
  packages = c("tibble", "tidyverse","here", "readxl"), # packages that your targets need to run
  format = "rds" # default storage format
  # Set other options as needed.
)

# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multiprocess")

# tar_make_future() configuration (okay to leave alone):
future::plan(future.callr::callr)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# source("other_functions.R") # Source other scripts as needed. # nolint

# Replace the target list below with your own:
list(
  tar_target(scraped_climbing
             ,here("data", "climbing_20220630.rds")
             ,format = "file")
  ,tar_target(ifsc_data_missing
              ,here("data", "ifsc_missing_tries_20220730.xlsx")
              ,format = "file")
  ,tar_target(ifsc_data_comb, ifsc_combine_data(scraped_climbing, ifsc_data_missing))
  ,tar_target(ifsc_data_comb_pro, ifsc_process_combined_data(ifsc_data_comb))
  ,tar_target(ifsc_data_comp_results, ifsc_comp_results(ifsc_data_comb_pro))
  ,tar_target(ifsc_data_comp_rank, ifsc_comp_results_rank(ifsc_data_comp_results))
  ,tar_target(ifsc_data_boulder_events, ifsc_boulder_events(ifsc_data_comb_pro))
  
   
  
  
)
