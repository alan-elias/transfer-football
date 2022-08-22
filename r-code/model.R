pacman::p_load(dplyr, janitor, readr, ggplot2, caret, factoextra, keras, leaps, vip, rpart, rpart.plot, ipred)

### Cluster ----
fifa_cluster <- fifa22 %>%
  select(sofifa_id, age, height_cm, weight_kg, preferred_foot, international_reputation,
         pace, shooting, passing, dribbling, defending, physic, attacking_heading_accuracy,
         goalkeeping_diving, first_11, wage, value)
fifa_cluster <- na.omit(fifa_cluster)
fifa_scale <- data.frame(scale(fifa_cluster[-1]))
fifa_scale <- na.omit(fifa_scale)
factoextra::fviz_nbclust(fifa_scale, kmeans ,method = "wss") +
  geom_vline(xintercept = 7, linetype = 2)

set.seed(123)
k <- kmeans(fifa_scale, 7)
#factoextra::fviz_cluster(k, fifa_scale)

result <- cbind(fifa_scale, cluster = k$cluster)
table(result$cluster)

result2 <- cbind(sofifa_id = fifa_cluster$sofifa_id, result, wage_real = fifa_cluster$wage, value_real = fifa_cluster$value)
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

# Model ----
load(".RData")
fifa_model <- fifa22 %>%
  select(sofifa_id, age, height_cm, weight_kg, potential, international_reputation,
         weak_foot, team_position, shooting, passing, dribbling, defending, physic, 
         attacking_heading_accuracy, goalkeeping_diving, wage, value)

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
