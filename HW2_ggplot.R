install.packages("ggplot2")

library(ggplot2)

df <- read.csv("E:/UT/term_3/statiscal inferense/HW/HW2/Foods.csv")
df
########### Q8 #################
############# part a ###########

df2<-subset(df, pricePerServing<100 ) #delete a row with outlier

ggplot(df2 , aes(pricePerServing)) + 
  geom_histogram(bins = 80 ,aes(y = ..count..))+
  ggtitle("Histogram of price per serving")+
  geom_density(lwd = 1,color = 2,aes(y = ..count..))

############ part b ###############
df$healthScore
df$readyInMinutes
ggplot(df, aes(x = readyInMinutes , y = healthScore ))+
  stat_density_2d(aes(fill = ..level..), geom = "polygon" , colour = 'white')+
  ggtitle("2D density plot of two variable")


########### part c ##########
t <- data.frame(sort(table(df$dishType)))
t

names(t)[1] <- 'Dish_types'
names(t)[2] <- 'Count'

ggplot(t , aes(x = Dish_types , y = Count))+
  geom_bar(position="dodge",stat="identity")+
  coord_flip()+
  ggtitle("Horizontal Bar Plot of dishType")
############### part d ###########
ggplot(data = df, aes(x= dishType, y = healthScore , fill = dishType))+
  geom_boxplot(size = 1)+
  ggtitle("boxplots of health-score")

############### part e ############
Table2= table(healthy=df$veryHealthy,dairyfree=df$dairyFree)
data.Proportion = data.frame(prop.table(Table2))
data.Proportion$Freq = round(data.Proportion$Freq,4)

install.packages("ggmosaic")
library(ggmosaic)




ggplot(data = data.Proportion) +
  geom_mosaic(aes(weight = Freq,x = product(dairyfree), fill = healthy)) +
  scale_y_continuous(limits =c(0,1))+
  annotate("text", x = 0.3, y = 0.45,label=paste(data.Proportion$Freq[[1]]*100,"%"),fontface = "bold" ,size=4,color="black")+
  annotate("text", x = 0.30, y = 0.90,label=paste(data.Proportion$Freq[[2]]*100,"%"), fontface = "bold",size = 4,color="black")+
  annotate("text", x = 0.80, y = 0.30,label=paste(data.Proportion$Freq[[3]]*100,"%"),fontface = "bold" ,size = 4,color="black")+
  annotate("text", x = 0.80, y = 0.80,label=paste(data.Proportion$Freq[[4]]*100,"%"), fontface = "bold",size = 4,color="black")+
  labs(title = "Mosaic plot",y="percent",x="dairyFree")+
  theme(plot.title = element_text(hjust = 0.5))
