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