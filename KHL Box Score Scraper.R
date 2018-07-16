library(tidyverse)
library(rvest)
library(splashr)
library(warrenr)

# Gets links for scoring summary info for each game of each season
get_schedule <- function(season) {
  
  if (season == "2017-18") {url = "https://en.khl.ru/calendar/468/00/"}
  
  else if (season == "2016-17") {url = "https://en.khl.ru/calendar/405/00/"}
  else if (season == "2015-16") {url = "https://en.khl.ru/calendar/309/00/"}
  else if (season == "2014-15") {url = "https://en.khl.ru/calendar/266/00/"}
  else if (season == "2013-14") {url = "https://en.khl.ru/calendar/244/00/"}
  else if (season == "2012-13") {url = "https://en.khl.ru/calendar/222/00/"}
  else if (season == "2011-12") {url = "https://en.khl.ru/calendar/202/00/"}
  else if (season == "2010-11") {url = "https://en.khl.ru/calendar/185/00/"}
  else if (season == "2009-10") {url = "https://en.khl.ru/calendar/167/00/"}
  else if (season == "2008-09") {url = "https://en.khl.ru/calendar/160/00/"}
  
  else {stop("Season not available. Sorry!")}
  
  schedule <- url %>%
    read_html() %>%
    html_nodes("ul+ ul li:nth-child(1) a") %>%
    html_attr("href") %>%
    str_c("https://en.khl.ru", .) %>%
    as_tibble() %>%
    set_names("url") %>%
    mutate(season = season)
  
  return(schedule)
}

seasons <- c("2008-09", "2009-10", 
             "2010-11", "2011-12", 
             "2012-13", "2013-14", 
             "2014-15", "2015-16", 
             "2016-17", "2017-18")

# Gets all the links into a data frame for all the seasons of interest
schedule <- map_df(seasons, get_schedule)
  
# Gets scoring summary box score info for specific game
get_box_score <- function(.row_num, .data) {
  
  selected_data <- .data %>% 
    filter(row_number() == .row_num)
  
  splash_container <- start_splash()
  on.exit(stop_splash(splash_container))
  
  Sys.sleep(runif(1, 10, 15))
  
  page <- splash_local %>%
    splash_response_body(TRUE) %>%
    splash_enable_javascript(TRUE) %>%
    splash_plugins(TRUE) %>%
    splash_user_agent(ua_win10_chrome) %>%
    splash_go(pull(selected_data, url)) %>%
    splash_wait(runif(1, 10, 15)) %>%
    splash_html()
  
  home_team <- page %>%
    html_nodes(".e-details_img+ .b-details_txt .e-club_name") %>%
    html_text()
  
  away_team <- page %>%
    html_nodes(".m-rightward .e-club_name") %>%
    html_text()
  
  box_score_data <- page %>% 
    html_node("#goals") %>% 
    html_table() %>% 
    set_names("goal_number", "period", "time", "score", "game_strength", "goal", "primary_assist", "secondary_assist", "home_team_on_ice", "away_team_on_ice") %>% 
    as_tibble() %>% 
    mutate_at(vars(goal, primary_assist, secondary_assist), ~str_replace_all(., c("[[:digit:]]" = "", "[[:punct:]]" = ""))) %>%
    mutate_all(~str_trim(., side = "both")) %>%
    mutate(home_score = str_split(score, "\\:", simplify = TRUE, n = 2)[,1]) %>%
    mutate(away_score = str_split(score, "\\:", simplify = TRUE, n = 2)[,2]) %>%
    mutate(goal = str_c(str_split(goal, " ", simplify = TRUE, n = 2)[,2], str_split(goal, " ", simplify = TRUE, n = 2)[,1], sep = " ")) %>%
    mutate(primary_assist = str_c(str_split(primary_assist, " ", simplify = TRUE, n = 2)[,2], str_split(primary_assist, " ", simplify = TRUE, n = 2)[,1], sep = " ")) %>%
    mutate(secondary_assist = str_c(str_split(secondary_assist, " ", simplify = TRUE, n = 2)[,2], str_split(secondary_assist, " ", simplify = TRUE, n = 2)[,1], sep = " ")) %>%
    mutate(season = pull(selected_data, season)) %>%
    mutate(home_team = home_team) %>%
    mutate(away_team = away_team) %>%
    mutate(team = case_when(home_score != lag(home_score, n = 1) ~ home_team,
                            away_score != lag(away_score, n = 1) ~ away_team,
                            pull(slice(., 1), home_score) == 1 ~ home_team,
                            pull(slice(., 1), away_score) == 1 ~ away_team)) %>%
    mutate_all(as.character) %>%
    mutate_all(~str_trim(., side = "both")) %>%
    select(team, game_strength, goal, primary_assist, secondary_assist, season)
  
  print(pull(selected_data, url))
  print(str_c(.row_num, "of", nrow(.data), sep = " "))
  return(box_score_data)
}

# If the above function fails, it will retry up to 10 times
persistently_get_box_score <- warrenr::persistently(get_box_score, max_attempts = 10, wait_seconds = 0.0001)

# If all 10 retries fail, the scraper will move on to the next game
try_get_box_score <- function(.row_num, .data) {
  
  tryCatch(persistently_get_box_score(.row_num, .data), 
           
           error = function(e) {
             print(e) 
             print(.row_num)
             data_frame()},
           
           warning = function(w) {
             print(w) 
             print(.row_num)
             data_frame()})
}

# Gets scoring summary box score info for each game of every season of interest
khl_box_score_data <- 1:nrow(schedule) %>% map_df(try_get_box_score, .data = schedule)
