pacman::p_load(ggplot2, dplyr, egg, GGally, scales, patchwork, corrplot)

options(scipen = 999)

### EDA ----
load(".RData")
pallete <- grDevices::colorRampPalette(c("seagreen", "skyblue"))
pallete <- grDevices::colorRampPalette(c("violet", "palevioletred4"))
cores <- pallete(4)
theme_set(theme_article(base_size = 12))

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
  filter(overall < 70) %>% 
  summarise(pace=mean(pace),
            chute=mean(shooting),
            passe = mean(passing),
            drible = mean(dribbling),
            defesa = mean(defending),
            fisico = mean(physic)) 

fifa22 %>% 
  group_by(first_11) %>%
  filter(overall < 75) %>%
  summarise(Salario=mean(wage),
            Valor=mean(value))

fifa22 %>% 
  group_by(position) %>% 
  summarise(PeFraco=mean(weak_foot)) %>% 
  ggplot(aes(x=position, y=PeFraco))+
  geom_col(fill = cores)+
  labs(y = "Pé Fraco", x = "Posição",
       caption = "Fonte: Dados do FIFA 22")+
  coord_cartesian(ylim = c(2,3.25))


fifa22 %>% 
  group_by(position) %>% 
  # filter(overall >= 80) %>% 
  summarise(reputation=mean(international_reputation)) %>% 
  ggplot(aes(x=position, y=reputation))+
  geom_col(fill = cores)+
  labs(y = "Reputação Internacional", x = "Posição",
       caption = "Fonte: Dados do FIFA 22")+
  # coord_cartesian(ylim = c(1,3))+
  # theme_bw()

# graph age mean
fifa22 %>% 
  group_by(position) %>%
  ggplot(aes(x=position, y=age))+
  geom_boxplot(fill = cores)+
  # coord_flip()+
  labs(y = "Idade", x = "Posição",
       caption = "Fonte: Dados do FIFA 22")

fifa22 %>% 
  group_by(position) %>%
  ggplot(aes(x=position, y=height_cm))+
  geom_violin()+
  labs(y = "Altura (cm)", x = "Posição",
       caption = "Fonte: Dados do FIFA 22")



fifa22 %>% 
  group_by(position, preferred_foot) %>%
  summarise(n=n()) %>%
  ggplot(aes(x=position, y=n, 
             fill=preferred_foot))+
  geom_bar(position = "dodge", stat = "identity")+
  labs(y = "", x = "", caption = "Fonte: Dados do FIFA 22", fill="Pé Dominante")+
  scale_fill_manual(values = c("palevioletred", "palevioletred4"),
                    labels = c("Esquerdo","Direito"))+ #changing chart labels and color
  theme_article()+
  theme(legend.position = "bottom")

fifa22 %>% 
  group_by(position) %>%
  ggplot(aes(x=position, y=weight_kg))+
  geom_violin(scale = "width")+
  labs(y = "Peso (kg)", x = "Posição", caption = "Fonte: Dados do FIFA 22")

fifa22 %>% 
  group_by(position) %>%
  ggplot(aes(x=weight_kg, y=height_cm))+
  geom_point(colour = cores[3])+
  labs(x = "Peso (kg)", y = "Altura", caption = "Fonte: Dados do FIFA 22")+
  theme_article()
#theme(element_text(size = 14))


# Using patchwork package (operator "/") for charts grouping
g.value_potential <- fifa22 %>% 
  group_by(position) %>%
  ggplot(aes(x=potential, y=value))+
  geom_point(colour = "orchid")+
  labs(y = "Valor de mercado", x = "Potencial", caption = "Fonte: Dados do FIFA 22")+
  scale_y_continuous(labels=dollar_format(suffix="",prefix="€",big.mark = ".", decimal.mark = ","))+
  theme_article()

g.wage_potential  <- fifa22 %>% 
  group_by(position) %>%
  ggplot(aes(x=potential, y=wage))+
  geom_point(colour = "palevioletred")+
  labs(y = "Salário", x = "Potencial", caption = "Fonte: Dados do FIFA 22")+
  scale_y_continuous(labels=dollar_format(suffix="",prefix="€",big.mark = ".", decimal.mark = ","))+
  theme_article()

g.wage_potential / g.value_potential

g.value <- ggplot(fifa22)+
  aes(x=value)+
  geom_density(col = "black", 
               fill = "palevioletred", 
               alpha = .5, 
               aes(y=..density..))+
  labs(x = "Valor", y = "Densidade", caption = "Fonte: Dados do FIFA 22")+
  scale_x_continuous(labels=dollar_format(suffix="",prefix="€",big.mark = ".", decimal.mark = ","))+
  theme_article()


g.logvalue <- ggplot(fifa22)+
  aes(x=log(value))+
  geom_density(col = "black", 
               fill = "palevioletred", 
               alpha = .5, 
               aes(y=..density..))+
  labs(x = "log(Valor)", y = "Densidade", caption = "Fonte: Dados do FIFA 22")+
  theme_article()

g.value / g.logvalue

fifa22 %>%
  ggplot(aes(y=age, x=first_11))+
  geom_violin(fill = cores[1])+
  labs(y = "Idade", x = "Iniciar Partida")+
  coord_flip()+
  coord_cartesian(ylim = c(16, 56), expand = T)+
  theme_article()


# Correlation plot

fifa_corr <- fifa22 %>% 
  select(age, height_cm, weight_kg, potential, international_reputation, weak_foot,
         team_position, shooting, passing, dribbling, wage, value) %>% 
  filter(team_position != 1 & value > 0)


# knitr::kable(cor(fifa_corr))
pairs(value~. , data = fifa_corr)

m <- cor(fifa_corr)
corrplot(m, order = "hclust", addrect = 3, tl.pos = "d", tl.cex = .65)
# car::spm(~.|team_position, data = fifa_corr)


#library(GGally)
# Cria grafico
fifa22[15:20] %>%
  ggpairs(columns = 6:1, ggplot2::aes(colour=fifa22$position))+
  labs(caption = "Fonte: Dados do FIFA 22")+
  scale_f(labels = c("Fisíco","Defesa","Drible","Passe","Chute","Pace"))




