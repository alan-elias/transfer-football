pacman::p_load(dplyr, janitor, readr, ggplot2, caret, factoextra, keras, leaps, vip, rpart, rpart.plot, ipred)

# Model ----
fifa22 <- read.csv('https://raw.githubusercontent.com/alan-elias/transfer-football/updates/r-code/sofifa.csv',
                   sep = ";", encoding = "UTF-8")


# fifa_model <- fifa22 %>%
#   select(sofifa_id, age, height_cm, weight_kg, potential, international_reputation,
#          weak_foot, team_position, shooting, passing, dribbling, defending, physic, 
#          attacking_heading_accuracy, goalkeeping_diving, wage, value)

fifa_model <- fifa22 %>%
  select(age, height, weight, foot, international_reputation,
         pac, sho, pas, dri, def, phy, gk_handling, value)

fifa_model <- na.omit(fifa_model)
# sample
set.seed(123)
extraction <- rsample::initial_split(fifa_model, prop = .7, strata = "value")
train <- rsample::training(extraction)
test  <- rsample::testing(extraction)

# Decision Tree

set.seed(123)
ad1 <- rpart(
  formula = value ~.,
  data = train,
  method = "anova")

options(scipen = 999)
rpart.plot(ad1)
plotcp(ad1)

#Sem penalidade
set.seed(123)
adsp = rpart(
  formula = value ~.,
  data = train,
  method = "anova",
  control = list(cp = 0, xval = 10)
)

rpart.plot(adsp)
plotcp(adsp)

# SSE
adsp$cptable

# cross valid

set.seed(123)
(adcv = train(
  value ~.,
  data = train,
  method = "rpart",
  trControl = trainControl("cv",5),
  tuneLength = 12)
)

ggplot(adcv)+theme_minimal()

vip::vip(adcv, num_features = 8, geom = "col")+theme_minimal()


p1 = predict(adcv, train)
sqrt(mean((train$value-p1)^2))

p2 = predict(adcv, test)
sqrt(mean((test$value-p2)^2))

data.gg = as.data.frame(cbind(
  id = 1:length(test$value),
  prediction = p2,
  test = test$value
))

ggplot(data.gg, aes(x=id,y=test))+
  geom_line(col="#f5ab35")+
  ylab("Valor de mercado")+xlab("")+
  geom_line(aes(x=id, y=prediction),col="#be90d4")+
  labs(caption = "Fonte: FIFA 22",legend = F)+
  theme_minimal()
