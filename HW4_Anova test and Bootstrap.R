###### Q3 ########
install.packages("palmerpenguins")
head(penguins)
library(palmerpenguins)
data(package = 'palmerpenguins')

df <- penguins[!(is.na(penguins$flipper_length_mm) |  is.na(penguins$species)),] 
 
######## a ##########
flipper_length <- c(penguins$flipper_length_mm)
species <- c(penguins$species)

######### b ##########
library(ggplot2)

ggplot(df) + 
  aes( x = species , y = flipper_length_mm , color =species )+
  geom_jitter() +
  theme(legend.position = "none")
  
######### c #########
# we assume that means be equal as null hypothesis(two side test)
#H0 : u1 = u2 = u3
#HA : at least two of them are not equal

x=unique(df$species)

#species 0 
ggplot(df[df$species==x[1],], aes(sample = flipper_length_mm))+
  stat_qq()+
  stat_qq_line()+
  labs(title = " 0 species",y="sumofscores")+
  theme(plot.title = element_text(hjust = 0.5))

#species 1 
ggplot(df[df$species==x[2],], aes(sample = flipper_length_mm))+
  stat_qq()+
  stat_qq_line()+
  labs(title = " 1 species",y="sumofscores")+
  theme(plot.title = element_text(hjust = 0.5))


#species 2 
ggplot(df[df$species==x[3],], aes(sample = flipper_length_mm))+
  stat_qq()+
  stat_qq_line()+
  labs(title = " 2 species",y="sumofscores")+
  theme(plot.title = element_text(hjust = 0.5))



##### Q 10 ######
car_train <- read.csv("E:/UT/term_3/statiscal inferense/HW/HW4/car_train.csv")
price <- na.omit(c(car_train$price))

####### a ##########
####### i #########
n <-100

sample_price <- sample(price , n ,replace = FALSE)
sample_price

actual_mean <- mean(price)
actual_mean

sample_mean <- mean(sample_price)
sample_mean
sample_std <- sd(sample_price)
sample_std


#mue = 15000
mue = 15000
z_score <- (sample_mean - mue) / (sample_std / sqrt(n))
z_score

p_value <- 2*pnorm(z_score ,lower.tail =FALSE)
p_value

########## ii ##########
CI <- c(sample_mean - qnorm(0.95+0.025)*sample_std/sqrt(n),
        sample_mean + qnorm(0.95+0.025)*sample_std/sqrt(n))
CI

############ iii ##########
z2 <- (sample_mean - mue)/(sample_std/sqrt(n))
z2

beta <- pnorm(z2 , lower.tail = TRUE)
beta
############ iv ############
#first approach
power <- pnorm(z2 , lower.tail = FALSE)
power
#second approach
power <- 1-beta
power

########### b################
df <-  na.omit(car_train[, c("price","odometer")])

sample_car <- df[sample(nrow(df), 25), ]

price <- c(sample_car$price)
odometer <- c(sample_car$odomete)


#H0: mean_price - mean_odometer = 0
#HA: mean_price - mean_odometer!= 0


mean_price = mean(price)
mean_odometer = mean(odometer)

sd_price = sd(price)
sd_odometer = sd(odometer)


SE =sqrt((sd_price^2/25 ) + (sd_odometer^2/25))

#degree of freedom
df = 24


T_df  = (mean_odometer-mean_price)/SE



p_value =  pt(T_df, df = df, lower.tail = FALSE)*2
p_value
