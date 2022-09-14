#__________ question 4 __________
#install.packages("openintro")
library(openintro)

absenteeism
#absenteeism$sex[absenteeism$sex == 'M']
levels(absenteeism$sex) <- c(1,0)
levels(absenteeism$eth) <- c(1,0)
levels(absenteeism$lrn) <- c(1,0)

model = lm(days ~ sex + eth + lrn , data = absenteeism)
model

summary(model)

hist(model$residuals , main="Histogram of Residuals")
qqnorm(model$residuals, main="Normal probability plot of Residuals")
qqline(model$residuals)

#___________ question 8 ___________
df <- data.frame(state.x77)

#### first step ##########
model <- lm(Life.Exp ~ ., data= df)
p_value <- summary(model)$coefficients[,4]
p_value

max(p_value)>0.05

del_feture_1 <- p_value[p_value == max(p_value)]
del_feture_1

###### second step ##########
model <- lm(Life.Exp ~ .- Area, data= df)
p_value <- summary(model)$coefficients[,4]
p_value

max(p_value)>0.05

del_feture_1 <- p_value[p_value == max(p_value)]
del_feture_1

###### tihrd step ########
model <- lm(Life.Exp ~ .- Area - Illiteracy , data= df)
p_value <- summary(model)$coefficients[,4]
p_value

max(p_value)>0.05

del_feture_1 <- p_value[p_value == max(p_value)]
del_feture_1

##### forth step ############
model <- lm(Life.Exp ~ .- Area - Illiteracy - Income , data= df)
p_value <- summary(model)$coefficients[,4]
p_value

max(p_value)>0.05

del_feture_1 <- p_value[p_value == max(p_value)]
del_feture_1

######### fifth step ###########3
model <- lm(Life.Exp ~ .- Area - Illiteracy - Income -Population , data= df)
p_value <- summary(model)$coefficients[,4]
p_value

max(p_value)>0.05

del_feture_1 <- p_value[p_value == max(p_value)]
del_feture_1

##### b######
summary(model)
