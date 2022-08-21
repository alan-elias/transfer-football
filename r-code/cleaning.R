### Libraries ----

#install.packages("pacman")
pacman::p_load(dplyr, janitor, readr, stringi, stringr, lubridate, ggplot2, caret, factoextra)

### Cleaning ----

options(scipen = 999)
players <- read.csv('https://raw.githubusercontent.com/alan-elias/transfer-football/updates/r-code/basic_info.csv',
                    sep = ",", encoding = "UTF-8") %>% 
  clean_names()

players$x <- NULL
players$total_stat <- NULL
names(players)[1] <-  'sofifa_id'

# Valor de mercado
players$value <- str_extract(players$value, "[0-9\\.[A-Z]+]+")
players$Unity <- str_detect(players$value,"M")
players$Unity <- if_else(players$Unity == "FALSE", 1000, 1000000)
players$value <- as.numeric(str_extract(players$value, "[0-9\\.]+"))
players$value <- players$value * players$Unity

# Salário
players$wage <- str_extract(players$wage, "[0-9\\.[A-Z]+]+")
players$UnityW <- str_detect(players$wage,"K")
players$UnityW <- if_else(players$UnityW == "FALSE", 1, 1000)
players$wage <- as.numeric(str_extract(players$wage, "[0-9]+"))
players$wage <- players$wage * players$UnityW

fifa22 <- read.csv("https://raw.githubusercontent.com/alan-elias/transfer-football/updates/r-code/players_22.csv",
                   sep = ",", encoding = "UTF-8") %>% 
  select("sofifa_id","short_name","player_positions", "dob", "height_cm", "weight_kg", "club_position", 
         "club_loaned_from","club_joined", "preferred_foot","international_reputation", "pace", "shooting", 
         "passing","dribbling","defending","physic", "attacking_heading_accuracy","goalkeeping_diving")

players_join <- players %>% select("sofifa_id", "value", "wage")
fifa22 <- right_join(players_join, fifa22, by = "sofifa_id") 
fifa22 <- fifa22 %>% relocate(c("wage", "value"),.after = goalkeeping_diving)

rm(players, players_join)


fifa22 <- fifa22 %>% 
  mutate(position = case_when(player_positions == "GK" ~ "GK",
                              str_detect(player_positions, "CB") ~ "CB",
                              str_detect(player_positions, "B") ~ "FB",
                              str_detect(player_positions, "AM") ~ "ATT_MID",
                              str_detect(player_positions, "RW") | str_detect(player_positions, "LW") 
                              | str_detect(player_positions, "RM")
                              | str_detect(player_positions, "LM") ~ "WING",
                              str_detect(player_positions, "M") ~ "MID",
                              str_detect(player_positions, "ST") | str_detect(player_positions, "CF") ~ "ST"),
         first_11 = ifelse(club_position %in% c("SUB", "RES", ""), FALSE, TRUE))

# XI - Starting -> 1 ; Reserve -> 0
fifa22$first_11 <- if_else(fifa22$first_11 == "TRUE", 1, 0)

# Name
# str_extract(fifa22$short_name, "[A-Z\\.]+")

# Recode positions
fifa22$position <- recode(fifa22$position,
                          "GK" = "Goleiro",
                          "CB" = "Zagueiro",
                          "FB" = "Lateral",
                          "MID" = "Meio-Campo",
                          "ATT_MID" = "Meia-Atacante",
                          "WING" = "Ponta",
                          "ST" = "Atacante")
#Age
fifa22$dob <- ymd(fifa22$dob)
fifa22$age <- as.numeric(trunc((Sys.Date() - fifa22$dob)/360))

fifa22 <- fifa22 %>% relocate(age, .after = dob)

# Preferred Foot - Right -> 1 ; Left -> 2
fifa22$preferred_foot <- if_else(fifa22$preferred_foot == "Right", 1, 2)

fifa22 <- fifa22 %>% relocate(c("position", "first_11"),.after = goalkeeping_diving)
