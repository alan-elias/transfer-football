pacman::p_load(ggplot2, ggridges, dplyr, egg, GGally, scales, patchwork, corrplot)

options(scipen = 999)

### EDA ----
fifa22 <- read.csv('https://raw.githubusercontent.com/alan-elias/transfer-football/updates/r-code/sofifa.csv',
                    sep = ";", encoding = "UTF-8") #18/08/2022
#Sahko info
fifa22[4085,c(4,16)] <- c("Atacante", 4)
fifa22$team_position <- as.numeric(fifa22$team_position)

# fifa_names <- c()

pallete <- grDevices::colorRampPalette(c("seagreen", "skyblue"))
pallete <- grDevices::colorRampPalette(c("violet", "palevioletred4"))
cores <- pallete(4)
theme_set(theme_article(base_size = 12))

cores <- c("#F77A2F", "#D4B128", "#28D493", "#227AF5")

fifa22 %>% 
  group_by(position) %>%
  filter(ova < 75) %>% 
  summarise(Media=mean(age),
            Mediana=median(age),
            Dp = sd(age),
            Var = var(age))

fifa22

fifa22 %>% 
  group_by(position) %>% 
  summarise(Media_Idade=mean(age),
            Media_Altura=round(mean(height),0),
            Media_Peso = mean(weight))

fifa22 %>% 
  group_by(position) %>% 
  summarise(pace=mean(pac),
            chute=mean(sho),
            passe = mean(pas),
            drible = mean(dri),
            defesa = mean(def),
            fisico = mean(phy))
fifa22 %>% 
  group_by(position) %>% 
  filter(overall < 70) %>% 
  summarise(pace=mean(pac),
            chute=mean(sho),
            passe = mean(pas),
            drible = mean(dri),
            defesa = mean(def),
            fisico = mean(phy)) 

fifa22 %>% 
  filter(ova < 75) %>%
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
       caption = "Fonte: Dados do FIFA 22")
  # coord_cartesian(ylim = c(1,3))+
  # theme_bw()

# graph age mean
fifa22 %>% 
  group_by(position) %>%
  ggplot(aes(x=position, y=age))+
  geom_boxplot(fill = cores)+
  coord_flip()+
  labs(y = "Idade", x = "Posição",
       caption = "Fonte: Dados do FIFA 22")+
  geom_vline(xintercept = mean(fifa22$age), linetype="dashed")

fifa22 %>%
  ggplot(aes(x=age))+
  geom_histogram(fill= "palevioletred", colour = "white")+
  labs(x = "Idade", y = "")+
  coord_cartesian(xlim = c(15,40))+
  geom_vline(xintercept = mean(fifa22$age), linetype="dashed")


fifa22 %>% 
  group_by(position) %>%
  ggplot(aes(x=position, y=height))+
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
  ggplot(aes(x=pot, y=value))+
  geom_point(colour = "orchid")+
  labs(y = "Valor de mercado", x = "Potencial", caption = "Fonte: Dados do FIFA 22")+
  scale_y_continuous(labels=dollar_format(suffix="",prefix="€",big.mark = ".", decimal.mark = ","))+
  theme_article()

g.wage_potential  <- fifa22 %>% 
  group_by(position) %>%
  ggplot(aes(x=pot, y=wage))+
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


# Correlation plot

fifa_corr <- fifa22 %>% 
  select(age, height, weight, pot, international_reputation, weak_foot,
         team_position, sho, pas, dri, wage, value)

  # filter(team_position != 1 & value > 0)


# knitr::kable(cor(fifa_corr))
pairs(value~. , data = fifa_corr)
names(fifa_corr) <-  c("idade", "altura", "peso", "potencial", "reputacao", "pe_fraco",
                       "posicao", "chute", "passe", "drible", "salario", "valor")

m <- cor(fifa_corr)
corrplot(m, type = "full", tl.cex = .65, tl.pos = "dt", tl.srt = 0, diag = F,
         order = "FPC", tl.col = "black", method = "number", number.digits = 2, number.cex = .9,
         addshade = c("negative", "positive", "all"))
corrplot(m, order = "hclust", addrect = 3, tl.pos = "d", tl.cex = .65)
# car::spm(~.|team_position, data = fifa_corr)

# 
# #library(GGally)
# # Cria grafico
# fifa22[17:22] %>%
#   ggpairs(columns = 6:1, ggplot2::aes(colour=fifa22$position))+
#   labs(caption = "Fonte: Dados do FIFA 22")+
#   scale_f(labels = c("Fisíco","Defesa","Drible","Passe","Chute","pac"))


fifa22 %>% 
  ggplot(aes(x=height, y=position))+
  geom_boxplot(fill=cores)+
  geom_vline(xintercept = mean(fifa22$height), linetype="dashed")+
  labs(x="Altura (cm)", y="") +
  theme_article()+
fifa22 %>% 
  ggplot(aes(x=weight, y=position))+
  geom_boxplot(fill=cores)+
  geom_vline(xintercept = mean(fifa22$weight), linetype="dashed")+
  labs(x="Peso (kg)", y="")+
  theme_article()


# players attributes chart
fifa22 %>% 
  ggplot(aes(x=pac, y=position))+
  geom_density_ridges(fill="palevioletred")+
  geom_vline(xintercept = mean(fifa22$pac), linetype="dashed")+
  labs(x="Pace", y="")+
  theme_article()+
fifa22 %>% 
  ggplot(aes(x=sho, y=position))+
  geom_density_ridges(fill="palevioletred")+
  geom_vline(xintercept = mean(fifa22$sho), linetype="dashed")+
  labs(x="Chute", y="")+
  theme_article()+
fifa22 %>% 
  ggplot(aes(x=pas, y=position))+
  geom_density_ridges(fill="palevioletred")+
  geom_vline(xintercept = mean(fifa22$pas), linetype="dashed")+
  labs(x="Passe", y="")+
  theme_article()+
fifa22 %>% 
  ggplot(aes(x=dri, y=position))+
  geom_density_ridges(fill="palevioletred")+
  geom_vline(xintercept = mean(fifa22$dri), linetype="dashed")+
  labs(x="Drible", y="")+
  theme_article()+
fifa22 %>% 
  ggplot(aes(x=def, y=position))+
  geom_density_ridges(fill="palevioletred")+
  geom_vline(xintercept = mean(fifa22$def), linetype="dashed")+
  labs(x="Defesa", y="")+
  theme_article()+
fifa22 %>% 
  ggplot(aes(x=phy, y=position))+
  geom_density_ridges(fill="palevioletred")+
  geom_vline(xintercept = mean(fifa22$phy), linetype="dashed")+
  labs(x="Físico", y="")+
  theme_article()

mean(fifa22$pac)
mean(fifa22$sho)
mean(fifa22$pas)
mean(fifa22$dri)
mean(fifa22$def)
mean(fifa22$phy)
