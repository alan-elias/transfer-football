library(dplyr)

options(scipen = 999)

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
