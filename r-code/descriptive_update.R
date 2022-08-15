### Libraries ----

library(dplyr) 
library(janitor)
library(readr) 
library(stringi)
library(stringr)
library(lubridate)
library(ggplot2)
library(caret)
#library(factoextra)

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

### EDA ----

pallete <- grDevices::colorRampPalette(c("seagreen", "skyblue"))
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
  summarise(Salario=mean(wage),
            Valor=mean(value))
fifa22 %>% 
  group_by(position) %>% 
  # filter(Overall < 75) %>% 
  summarise(Salario=mean(wage),
            Valor=mean(value)) 
fifa22 %>% 
  group_by(first_11) %>%
  # filter(Overall >= 75) %>%
  summarise(Salario=mean(wage, na.rm=T),
            Valor=mean(value, na.rm=T)) 
fifa22 %>% 
  group_by(position) %>%
  # filter(Overall >= 75) %>%
  summarise(Salario=mean(wage, na.rm=T),
            Valor=mean(value, na.rm=T)) 

fifa22 %>% 
  group_by(first_11) %>%
  filter(Overall < 75) %>%
  summarise(Salario=mean(wage),
            Valor=mean(value)) 
fifa22 %>% 
  group_by(first_11) %>%
  filter(ContractDuration > 8) %>% 
  summarise(tempo=mean(ContractDuration, na.rm = T)) 

fifa22 %>% 
  group_by(position) %>% 
  summarise(PeFraco=mean(weak_foot)) %>% 
  ggplot(aes(x=position, y=PeFraco))+
  geom_col(fill = cores)+
  labs(y = "Pé Fraco", x = "Posição",
       caption = "Fonte: Dados do FIFA 22")+
  coord_cartesian(ylim = c(2,3.25))+
  theme_bw()

fifa22 %>% 
  group_by(position) %>% 
  summarise(MovHab=mean(skill_moves)) %>% 
  ggplot(aes(x=position, y=MovHab))+
  geom_col(fill = cores)+
  labs(y = "Movimentos de Habilidade", x = "Posição",
       caption = "Fonte: Dados do FIFA 22")+
  coord_cartesian(ylim = c(1,3))+
  theme_bw()

fifa22 %>% 
  group_by(position) %>% 
  filter(Overall >= 80) %>% 
  summarise(reputation=mean(international_reputation)) %>% 
  ggplot(aes(x=position, y=reputation))+
  geom_col(fill = cores)+
  labs(y = "Reputação Internacional", x = "Posição",
       caption = "Fonte: Dados do FIFA 22")+
  coord_cartesian(ylim = c(1,3))+
  theme_bw()

# graph age mean
fifa22 %>% 
  group_by(position) %>%
  ggplot(aes(x=position, y=age))+
  geom_boxplot(fill = cores)+
  coord_flip()+
  labs(y = "Idade", x = "Posição",
       caption = "Fonte: Dados do FIFA 22")+
  theme_bw()

fifa22 %>% 
  group_by(position) %>%
  ggplot(aes(x=position, y=height_cm))+
  geom_violin()+
  labs(y = "Altura (cm)", x = "Posição")+
  theme_bw()



fifa22 %>% 
  group_by(position, preferred_foot) %>%
  summarise(n=n()) %>% 
  ggplot(aes(x=position, y=n, fill=preferred_foot))+
  geom_bar(stat = "identity")+
  labs(y = "", x = "")+
  theme_bw()+
  theme(legend.position = "bottom")

fifa22 %>% 
  group_by(position) %>%
  ggplot(aes(x=position, y=weight_kg))+
  geom_violin(scale = "width")+
  labs(y = "Peso (kg)", x = "Posição")+
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


#library(GGally)
# Cria grafico
fifa22[13:18] %>%
  GGally::ggpairs(columns = 6:2, ggplot2::aes(colour=fifa22$position))+
  theme_bw()




### Model ----
fifa_model <- fifa22 %>%
  select(sofifa_id, age, height_cm, weight_kg, preferred_foot, international_reputation,
         pace, shooting, passing, dribbling, defending, physic, attacking_heading_accuracy,
         goalkeeping_diving, first_11, wage, value)
fifa_model <- na.omit(fifa_model)
fifa_scale <- data.frame(scale(fifa_model[-1]))
fifa_scale <- na.omit(fifa_scale)
factoextra::fviz_nbclust(fifa_scale, kmeans ,method = "wss") +
  geom_vline(xintercept = 7, linetype = 2)

set.seed(123)
k <- kmeans(fifa_scale, 7)
#factoextra::fviz_cluster(k, fifa_scale)

result <- cbind(fifa_scale, cluster = k$cluster)
table(result$cluster)

result2 <- cbind(sofifa_id = fifa_model$sofifa_id, result, wage_real = fifa_model$wage, value_real = fifa_model$value)
result_join <- inner_join(fifa22, result2[,c(1,18)], by = "sofifa_id")
cluster1_join<-result_join %>% filter(.,cluster==1)


cluster1<-result2 %>% filter(.,cluster==1)
cluster2<-result2 %>% filter(.,cluster==2) 
cluster3<-result2 %>% filter(.,cluster==3) 
cluster4<-result2 %>% filter(.,cluster==4) 
cluster5<-result2 %>% filter(.,cluster==5) 
cluster6<-result2 %>% filter(.,cluster==6) 
cluster7<-result2 %>% filter(.,cluster==7) 

summary(cluster1$value_real)
summary(cluster2$value_real)
summary(cluster3$value_real)
summary(cluster4$value_real)
summary(cluster5$value_real)
summary(cluster6$value_real)
summary(cluster7$value_real)

ggplot(data=result2,aes(x="",y=value_real))+
  geom_boxplot()+xlab("")+ylab("")+
  geom_jitter(alpha = 0.1, width = 0.2,colour="red")+
  facet_wrap(.~cluster,scales = 'free') +theme_minimal()

ggplot(data=result2,aes(x=value_real,y=..density..))+
  geom_density(fill="red",colour="red",alpha = 0.2)+
  xlab("")+ylab("")+
  facet_wrap(.~cluster,scales = 'free') +theme_minimal()


# gap_stat <- cluster::clusGap(fifa_scale, FUN = kmeans, nstart = 25,
#                     K.max = 10, B = 10)
# print(gap_stat, method = "firstmax")
# factoextra::fviz_gap_stat(gap_stat)
