get_skater_seasons <- function(season, rates=TRUE) {
  require(rvest)
  require(tidyverse)
  require(janitor)
  
  # Scrape from hockey-reference.com
  data <- read_html(paste("https://www.hockey-reference.com/leagues/NHL_",
                          season, "_skaters.html", sep = "")) |>
    html_node(xpath='//*[@id="player_stats"]') |>
    html_table()
  
  # Fix column names
  data <- data %>%
    row_to_names(1) %>%
    select(-c(Rk, Awards)) %>%
    filter(Player != "League Average") %>%
    rename_all(tolower) %>%
    rename(eva = "ev", ppa = "pp", sha = "sh", blk = "bl",
           plus_minus = "+/-", fo_pct = "fo%") %>%
    filter(pos != 'G')
  
  duplicates <- data %>% filter(grepl("TM", team)) %>% pull(player)
  data <- data %>%
    mutate(name = if_else(player == "Sebastian Aho",
                          if_else(pos == "D",
                                  "5ebastian Aho",
                                  "Sebastian Aho"),
                          player),
           year = season,
           .before = "player") %>%
    # Remove split seasons
    filter(!(player %in% duplicates) | (grepl("TM", team))) %>%
    select(-player) %>%
    # Fix Datatypes
    mutate(across(c(year, age, gp:tsa, fow:give), as.numeric),
           across(toi:atoi, ~ time_length(ms(.x), unit="min")))
  
  if (rates) {
    data <- calculate_rates(data)
  }
  return(data)
}

calculate_rates <- function(data) {
  require(dplyr)
  data_rates <- data %>%
    mutate(across(c(g:sog, tsa, fow:fol, blk:give), ~ 60*(.x / toi),
                  .names = "{.col}_per60"))
}

get_mult_skater_seasons <- function(start_season, end_season, colwise=TRUE, rates=TRUE) {
  mult_seasons_data <- get_skater_seasons(end_season, rates=rates)
  n_seasons <- end_season - start_season + 1
  if (n_seasons > 1) {  
    for (i in 1:(n_seasons-1)) {
      temp <- get_skater_season(end_season - i, rates=rates)
      Sys.sleep(0.5)
      mult_seasons_data <- mult_seasons_data %>%
        left_join(temp, by = "name", suffix = c("", paste(".n-", i, sep = "")))
    }
  }
  return(mult_seasons_data)
}
