## WebScraping ----
pacman::p_load(rvest, dplyr, stringr, lubridate, tidyr, janitor)

page <- seq(0,19260,60)
link <- paste0("https://sofifa.com/players?showCol%5B0%5D=pi&showCol%5B1%5D=ae&showCol%5B2%5D=hi&showCol%5B3%5D=wi&showCol%5B4%5D=pf&showCol%5B5%5D=oa&showCol%5B6%5D=pt&showCol%5B7%5D=bp&showCol%5B8%5D=gu&showCol%5B9%5D=vl&showCol%5B10%5D=wg&showCol%5B11%5D=gh&showCol%5B12%5D=wk&showCol%5B13%5D=ir&showCol%5B14%5D=pac&showCol%5B15%5D=sho&showCol%5B16%5D=pas&showCol%5B17%5D=dri&showCol%5B18%5D=def&showCol%5B19%5D=phy&offset=",page)

# xpath <- //*[@id="body"]/div[1]/div/div[2]/div/table
read_html(iconv(page_source[[1]], to = "UTF-8"), encoding = "utf8")
get_table <- function(url){
  url %>% 
    read_html(encoding="utf-8") %>% 
    html_nodes(xpath = '//*[@id="body"]/div[1]/div/div[2]/div/table') %>% 
    html_table(header = T)
}

# Applying the created function to URLs
scraping <- sapply(link, get_table)

# Joining scraping a data frame
data <- do.call(rbind, scraping)
data <- as.data.frame(data)


## Cleaning ----
sofifa <- data
sofifa <- clean_names(sofifa)
sofifa[1] <- NULL
names(sofifa)[5] <- "team"
names(sofifa)[6] <- "sofifa_id"
names(sofifa)[10] <- "best_position"
names(sofifa)[15] <- "weak_foot"
names(sofifa)[16] <- "international_reputation"

sofifa <-  sofifa %>% relocate(sofifa_id, .before = "name")



# Position
sofifa <- sofifa %>% 
  mutate(position = case_when(best_position == "GK" ~ "GK",
                              str_detect(name, "CB") ~ "CB",
                              str_detect(name, "B") ~ "FB",
                              str_detect(name, "AM") ~ "ATT_MID",
                              str_detect(name, "RW") | str_detect(name, "LW") 
                              | str_detect(name, "RM")
                              | str_detect(name, "LM") ~ "WING",
                              str_detect(name, "M") ~ "MID",
                              str_detect(name, "ST") | str_detect(name, "CF") ~ "ST"))


sofifa$position <- recode(sofifa$position,
                          "GK" = "Goleiro",
                          "CB" = "Defensor",
                          "FB" = "Defensor",
                          "MID" = "Meio-Campo",
                          "ATT_MID" = "Atacante",
                          "WING" = "Atacante",
                          "ST" = "Atacante") #Pt-br

sofifa$team_position <- recode(sofifa$position,
                               "Goleiro" = 1,
                               "Defensor" = 2,
                               "Meio-Campo" = 3,
                               "Atacante" = 4) #Pt-br


# Name

#name_prefix <- str_extract(sofifa$name, "[A-Z]\\.+") %>%  replace_na("")
#name_sufix <- str_extract(sofifa$name, "[A-Z][a-z]+")
remove_symbol <- function(x) iconv(x, to = "ASCII//TRANSLIT")
sofifa$name <- remove_symbol(sofifa$name)
name_extract <- str_extract(sofifa$name, "[^-[:digit:]]+[a-z]+")
sofifa$name <- name_extract

# Team name
team_split <- str_split(sofifa$team, "~")
team_split <- sapply(team_split, "[[" , 1)
team_split <- str_split(team_split, "\n")
team_split <- sapply(team_split, "[[" , 1)
sofifa$team <- team_split

#  Extract numbers
sofifa$height <- str_extract(sofifa$height, "[0-9]+") #cm
sofifa$weight <- str_extract(sofifa$weight, "[0-9]+") #kg


# reorder variables
sofifa <- sofifa %>% relocate(team, .after = "name")
sofifa <- sofifa %>% relocate(position, .after = "team")
sofifa <- sofifa %>% relocate(foot, .after = "position")
sofifa <- sofifa %>% relocate(best_position, .after = "foot")
sofifa <- sofifa %>% relocate(height, .after = "best_position")
sofifa <- sofifa %>% relocate(weight, .after = "height")
sofifa <- sofifa %>% relocate(team_position, .after = "international_reputation")


sofifa <- sofifa %>% relocate(wage, .after = "phy")
sofifa <- sofifa %>% relocate(value, .after = "wage")

# Market value
sofifa$value <- str_extract(sofifa$value, "[0-9\\.[A-Z]+]+")
sofifa$Unity <- str_detect(sofifa$value,"M")
sofifa$Unity <- if_else(sofifa$Unity == "FALSE", 1000, 1000000)
sofifa$value <- as.numeric(str_extract(sofifa$value, "[0-9\\.]+"))
sofifa$value <- sofifa$value * sofifa$Unity

# wage
sofifa$wage <- str_extract(sofifa$wage, "[0-9\\.[A-Z]+]+")
sofifa$UnityW <- str_detect(sofifa$wage,"K")
sofifa$UnityW <- if_else(sofifa$UnityW == "FALSE", 1, 1000)
sofifa$wage <- as.numeric(str_extract(sofifa$wage, "[0-9]+"))
sofifa$wage <- sofifa$wage * sofifa$UnityW

sofifa[25:26] <- NULL
rm(team_split, name_extract)

# export data
write.table(format(sofifa, scientific=F), 'sofifa.csv', 
            sep = ";", dec = ",", 
            row.names = F, quote = F, 
            fileEncoding = "UTF-8")
