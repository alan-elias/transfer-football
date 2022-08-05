library(dplyr)

options(scipen = 999)

### Scraping market value and wage ----
players <- read.csv("basic_info.csv", header = T, sep = ",")
players$X <- NULL
names(players)[1] <-  'sofifa_id'

# Valor de mercado
players$Value <- str_extract(players$Value, "[0-9\\.[A-Z]+]+")
players$Unity <- str_detect(players$Value,"M")
players$Unity <- if_else(players$Unity == "FALSE", 1000, 1000000)
players$Value <- as.numeric(str_extract(players$Value, "[0-9\\.]+"))
players$Value <- players$Value * players$Unity

# Salário
players$Wage <- str_extract(players$Wage, "[0-9\\.[A-Z]+]+")
players$UnityW <- str_detect(players$Wage,"K")
players$UnityW <- if_else(players$UnityW == "FALSE", 1, 1000)
players$Wage <- as.numeric(str_extract(players$Wage, "[0-9]+"))
players$Wage <- players$Wage * players$UnityW

fifa22 <- read.csv("players_22.csv", header = T, sep = ",") %>% 
  select("sofifa_id","short_name","player_positions", "dob", "height_cm", "weight_kg", "club_position", 
         "club_loaned_from","club_joined", "preferred_foot","international_reputation", "pace", "shooting", 
         "passing","dribbling","defending","physic", "attacking_heading_accuracy","goalkeeping_diving")

players_join <- players %>% select("sofifa_id", "Value", "Wage")
fifa22 <- right_join(players_join, fifa22, by = "sofifa_id") 
fifa22 <- fifa22 %>% relocate(c("Wage", "Value"),.after = goalkeeping_diving)

rm(players, players_join)

fifa22 <- janitor::clean_names(fifa22)
### Cleaning ----
fifa22 <- fifa22 %>% 
  mutate(position = case_when(player_positions == "GK" ~ "GK",          # Broader position category
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

### Tables and Graphics ----

pallete <- colorRampPalette(c("seagreen", "cian"))
cores <- pallete(7)


fifa22 %>% 
  group_by(position) %>% 
  summarise(Media=mean(age),
            Mediana=median(age),
            Dp = sd(age),
            Var = var(age))

fifa22 %>% 
  group_by(position) %>% 
  summarise(Media_Idade=mean(age),
            Media_Altura=round(mean(height_cm),0),
            Media_Peso = mean(weight_kg))

fifa22 %>% 
  group_by(position) %>% 
  summarise(pace=mean(pace),
            chute=mean(shooting),
            passe = mean(passing),
            drible = mean(dribbling),
            defesa = mean(defending),
            fisico = mean(physic)) 
fifa22 %>% 
  group_by(position) %>% 
  filter(Overall < 70) %>% 
  summarise(pace=mean(pace),
            chute=mean(shooting),
            passe = mean(passing),
            drible = mean(dribbling),
            defesa = mean(defending),
            fisico = mean(physic)) 
fifa22 %>% 
  group_by(position) %>% 
  # filter(Overall >= 75) %>% 
  summarise(Salario=mean(Wage),
            Valor=mean(Value))
fifa22 %>% 
  group_by(position) %>% 
  # filter(Overall < 75) %>% 
  summarise(Salario=mean(Wage),
            Valor=mean(Value)) 
fifa22 %>% 
  group_by(first_11) %>%
  # filter(Overall >= 75) %>%
  summarise(Salario=mean(Wage),
            Valor=mean(Value)) 

fifa22 %>% 
  group_by(first_11) %>%
  filter(Overall < 75) %>%
  summarise(Salario=mean(Wage),
            Valor=mean(Value)) 
fifa22 %>% 
  group_by(first_11) %>%
  filter(ContractDuration > 8) %>% 
  summarise(tempo=mean(ContractDuration, na.rm = T)) 

fifa22 %>% 
  group_by(position) %>% 
  summarise(PeFraco=mean(weak_foot)) %>% 
  ggplot(aes(x=position, y=PeFraco))+
  geom_col(fill = cores)+
  labs(y = "Pé Fraco", x = "positionição",
       caption = "Fonte: Dados do FIFA 22")+
  coord_cartesian(ylim = c(2,3.25))+
  theme_bw()

fifa22 %>% 
  group_by(position) %>% 
  summarise(MovHab=mean(skill_moves)) %>% 
  ggplot(aes(x=position, y=MovHab))+
  geom_col(fill = cores)+
  labs(y = "Movimentos de Habilidade", x = "positionição",
       caption = "Fonte: Dados do FIFA 22")+
  coord_cartesian(ylim = c(1,3))+
  theme_bw()

fifa22 %>% 
  group_by(position) %>% 
  filter(Overall >= 80) %>% 
  summarise(reputation=mean(international_reputation)) %>% 
  ggplot(aes(x=position, y=reputation))+
  geom_col(fill = cores)+
  labs(y = "Reputação Internacional", x = "positionição",
       caption = "Fonte: Dados do FIFA 22")+
  coord_cartesian(ylim = c(1,3))+
  theme_bw()

# graph age mean
fifa22 %>% 
  group_by(position) %>%
  ggplot(aes(x=position, y=age))+
  geom_boxplot(fill = cores)+
  coord_flip()+
  labs(y = "Idade", x = "positionição",
       caption = "Fonte: Dados do FIFA 22")+
  theme_bw()

fifa22 %>% 
  group_by(position) %>%
  ggplot(aes(x=position, y=height_cm))+
  geom_violin()+
  labs(y = "Altura (cm)", x = "positionição")+
  theme_bw()



fifa22 %>% 
  group_by(position, preferred_foot) %>%
  summarise(n=n()) %>% 
  ggplot(aes(x=position, y=n, fill=preferred_foot))+
  geom_bar(stat = "identity")+
  labs(y = "", x = "")+
  theme_bw()+
  theme(legend.positionition = "bottom")

fifa22 %>% 
  group_by(position) %>%
  ggplot(aes(x=position, y=weight_kg))+
  geom_violin(scale = "width")+
  labs(y = "Peso (kg)", x = "positionição")+
  theme_bw()

fifa22 %>% 
  group_by(position) %>%
  ggplot(aes(x=weight_kg, y=height_cm))+
  geom_point(colour = cores[3])+
  labs(y = "Peso (kg)", x = "Altura")+
  theme_bw()
#theme(element_text(size = 14))

fifa22 %>%
  ggplot(aes(y=age, x=first_11))+
  geom_violin(fill = cores[1])+
  labs(y = "Idade", x = "Iniciar Partida")+
  coord_flip()+
  coord_cartesian(ylim = c(16, 56), expand = T)+
  theme_bw()
#theme(element_text(size = 14))


library(GGally)

# Cria grafico
fifa22[13:18] %>%
  ggpairs(columns = 6:2, ggplot2::aes(colour=fifa22$position))+
  theme_bw()

### Model ----
# fifa_model <- fifa22 %>% 
#   select()