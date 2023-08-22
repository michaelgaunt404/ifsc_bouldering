#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# This is script scrapes the IFSC data.
#
# By: mike gaunt, michael.gaunt@wsp.com
#
# README: this script is currently unformatted 
#-------- i believe this script was the correct one grabbed from the surface CPU
#
# *please use 80 character margins
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




library(rvest)
library(XML)
library(RSelenium)
library(tidyverse)
library(data.table)
library(lubridate)

driver <- rsDriver(browser = 'firefox', port = 4444L)
server <- driver$server
browser <- driver$client

url = "https://www.ifsc-climbing.org/index.php/world-competition/last-result"
browser$navigate(url)

iframes = browser$findElements(using ="tag name", "iframe")
browser$switchToFrame(iframes[[1]])

scraped_data = seq(1, 15) %>%
  map(~{

    browser$findElement(using = "xpath", str_glue("//select[@id = 'years']/option[{.x}]"))$clickElement()

    Sys.sleep(2)

    browser$findElement(using = "xpath", "//select[@id = 'indexes']/option[2]")$clickElement()

    Sys.sleep(2)

    scrape_object =  browser$getPageSource()[[1]]  %>%
      read_html() %>%
      # .[[1]]
      rvest::html_nodes("select") %>%
      .[3]

    scraped_data = data.frame(event_name = scrape_object %>%
                                html_children() %>%
                                html_text()
                              ,event_id = scrape_object %>%
                                html_children() %>%
                                html_attr("value") %>%
                                gsub(".*events/", "\\1", .) %>%
                                as.numeric())
  })

scraped_data_df = scraped_data %>%
  reduce(bind_rows) %>%
  na.omit() %>%
  filter(str_detect(event_name, "\\(B\\)|B,|\\(B,") == T ) %>%
  filter(str_detect(event_name, "CANCELLED") == F) %>%
  mutate(event_name = str_trim(event_name))

get_to_data = function(scrape_object){
  scrape_object %>%
    httr::content(type = "text") %>%
    jsonlite::fromJSON()
}

safe_get_to_data = safely(get_to_data)

query_and_clean = function(event_id, gender){

  temp = httr::GET(str_glue("https://components.ifsc-climbing.org/results-api.php?api=event_full_results&result_url=/api/v1/events/{event_id}/result/{gender}")) %>%
    safe_get_to_data() %>%
    .$result %>%
    .$ranking

  temp_top_level = temp %>%
    .[,-9]

  temp_bottom_level = temp %>%
    .[,9] %>%
    map(~{
      list(.x$category_round_id,
           .x$round_name,
           .x$ascents,
           # .x$speed_elimination_stages$ascents,
           .x$score) %>%
        pmap(function(x, y, z, m)
          data.frame(category_round_id = x,
                     round_name = y,
                     score = m,
                     z )
        ) %>%
        reduce(bind_rows)
    })

  temp_complete = list(
    temp_top_level$athlete_id
    ,temp_bottom_level
  ) %>%
    pmap(~{
      .y %>%
        # .[[1]] %>%
        mutate(athlete_id = .x)

    }) %>%
    reduce(bind_rows) %>%
    merge(temp_top_level,
          by = "athlete_id") %>%
    mutate(event_id = event_id,
           gender_id = gender)

}

safe_query_and_clean = safely(query_and_clean)

full_climbing_data_lists_test = expand.grid(scraped_data_df$event_id %>%  head(12)
                                            ,c(3, 7)) %>%
  pmap(~{
    Sys.sleep(3)
    safe_query_and_clean(.x, .y)[['result']]
  })

full_climbing_data = full_climbing_data_lists %>%
  reduce(bind_rows) %>%
  merge(scraped_data_df, by = "event_id")


full_climbing_data_test = full_climbing_data_lists_test %>%
  reduce(bind_rows) %>%
  merge(scraped_data_df, by = "event_id")

c(
  full_climbing_data
  ,full_climbing_data_test
) %>%
  map(colnames)
full_climbing_data %>%  colnames()
full_climbing_data_test %>% colnames()

climbing_comb = bind_rows(
  full_climbing_data
  ,full_climbing_data_test
) %>%
  janitor::remove_empty("cols") %>%
  mutate(year = str_remove_all(event_name, "[:punct:]|[:alpha:]") %>%
           as.numeric()) %>%
  group_by(event_id, athlete_id, category_round_id) %>%
  mutate(index_og_order = row_number()) %>%
  arrange(route_id) %>%
  mutate(index_route_order = row_number()) %>%
  ungroup() %>%
  arrange(desc(year), event_id, rank)



saveRDS(climbing_comb, "./data/climbing_20220630.rds")

#evenly distributed across years, go with route_id
climbing_comb %>%
  filter(index_route_order != index_og_order) %>%
  count(year)

climbing_comb %>%
  group_by(event_id, athlete_id, category_round_id) %>%
  arrange(desc(year), event_id, category_round_id, rank, index_route_order) %>%
  mutate(top_cum = cumsum(top)
         ,zone_cum = cumsum(zone)) %>%
  mutate(top_ttl_att = sum(top_tries),
         zone_ttl_att = sum(zone_tries)) %>%
  head(10) %>%
  glimpse()



climbing_comb %>%
  filter(index_route_order != index_og_order) %>%
  count(year)





#expolre-------

climbing_comb = climbing_comb %>%
  data.table()


climbing_comb_results = climbing_comb %>%
  select(year, athlete_id, round_name, country, gender_id, rank) %>%
  unique()


climbing_comb


















# x = 1047
#
# yolo = scraped_data_df$event_id %>%
#   map(~{
#
#     Sys.sleep(2)
#
#     temp = httr::GET(str_glue("https://components.ifsc-climbing.org/results-api.php?api=event_full_results&result_url=/api/v1/events/{x}/result/7")) %>%
#       safe_get_to_data() %>%
#       .$result %>%
#       .$ranking
#
#     temp_top_level = temp %>%
#       .[,-9]
#
#     temp_bottom_level = temp %>%
#       .[,9] %>%
#       map(~{
#         list(.x$category_round_id,
#              .x$round_name,
#              .x$speed_elimination_stages$ascents) %>%
#           pmap(function(x, y, z)
#             data.frame(category_round_id = x,
#                        round_name = y,
#                        z )
#           ) %>%
#           reduce(bind_rows)
#       })
#
#     temp_complete = list(
#       temp_top_level$athlete_id %>%   head(5)
#       ,temp_bottom_level %>%   head(5)
#     ) %>%
#       pmap(~{
#         .y %>%
#           # .[[1]] %>%
#           mutate(athlete_id = .x)
#
#       }) %>%
#       reduce(bind_rows) %>%
#       merge(temp_top_level,
#             by = "athlete_id")
#
#
#
#
#     women_bottom_level = women %>%
#       .[1, 9] %>%
#       .[[1]]
#       map(~{
#         # .x$category_round_id
#         # temp_1 =
#         .x %>%
#           .[,c(1:4)]
#         # %>%
#           # .$speed_elimination_stages %>%
#           # .$ascents
#           # %>%
#         #   mutate(category_round_id = .x$category_round_id
#         #          ,round_name      = .x$round_name
#         #          ,score = .x$score)
#
#       }) %>%
#       reduce(bind_rows)
#
#
#    temo =  women$result$ranking %>%
#      .[,-9]
#       .[c(1:2), -9] %>%
#      .[[1]]
#
#    temo$speed_elimination_stages$ascents %>%
#      reduce(bind_rows)
#
#
#    temo %>%
#      str()
#
#    temo %>%
#      map(~{.x %>%
#          # print()
#          .$speed_elimination_stages %>%
#          .$ascents %>%
#          reduce(bind_rows)
#
#      })
#    temo$speed_elimination_stages$ascents %>%
#      reduce(bind_rows)
#       .$
#       dim()
#
#     men = httr::GET(str_glue("https://components.ifsc-climbing.org/results-api.php?api=event_full_results&result_url=/api/v1/events/{.x}/result/3")) %>%
#       safe_get_to_data() %>%
#       .$result
#
#     object = list(women, men)
#
#     object
#
#   })
#
# # yolo  %>%
# #   write_rds("C:/Users/rebec/Documents/Projects/zz_burner/workbench/data/scraped_climbing_20220628.rds")
#
# "C:/Users/rebec/Documents/Projects/zz_burner/workbench/data/scraped_climbing_20220628"
#
# yolo %>%
#   .[[1]] %>%
#   .[2]
#   length()
#   .["ranking"]
#
#
#
#
#
#
#
#
#
#   yolo = query_and_clean(1047, 7)
#
#   yolo = safe_query_and_clean(1047, 7)[['result']]
#
#
#
#
#
#
#   test %>%  count(event_id)
#
#   test %>%  count(athlete_id)
#
#   test %>%  count(rank)
#
#   test %>%
#     select(contains("name"), rank, gender_id) %>%
#     unique() %>%
#     arrange(event_name, gender_id, rank)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
